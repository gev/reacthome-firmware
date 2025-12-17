module Feature.AOutputs where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Context
import Core.Domain qualified as D
import Core.Task
import Core.Transport qualified as T
import Data.Buffer
import Data.Fixed
import Data.Index
import Data.Serialize
import Data.Value hiding (value)
import Endpoint.AOutputs qualified as E
import GHC.TypeNats
import Interface.DAC qualified as I
import Interface.MCU
import Ivory.Language
import Ivory.Stdlib

data AOutputs n
    = forall a.
      (I.DAC a) =>
    AOutputs
    { n :: Uint8
    , getAOutputs :: E.AOutputs n
    , getDACs :: List n a
    , shouldInit :: Value IBool
    , current :: Index Uint8
    , transmit ::
        forall l.
        (KnownNat l) =>
        Buffer l Uint8 ->
        forall s t.
        Ivory (ProcEffects s t) ()
    }

aoutputs ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , T.Transport t
    , I.DAC a
    , KnownNat n
    ) =>
    List n (p -> m a) ->
    t ->
    m (AOutputs n)
aoutputs dacs transport = do
    mcu <- asks D.mcu
    shouldInit <- asks D.shouldInit
    as <- traverse ($ peripherals mcu) dacs
    let n = length as
    getAOutputs <- E.mkAOutputs "aoutputs"
    current <- index "current_analog"

    let aoutputs =
            AOutputs
                { n = fromIntegral n
                , getAOutputs
                , getDACs = as
                , shouldInit
                , current
                , transmit = T.transmitBuffer transport
                }

    addSync "analog_aoutputs" $ forceSync aoutputs
    addTask $ yeld "analog_aoutputs_sync" $ sync aoutputs
    addTask $ yeld "analog_aoutputs_manage" $ manage aoutputs

    pure aoutputs

manage :: (KnownNat n) => AOutputs n -> Ivory eff ()
manage AOutputs{..} = zipWithM_ zip getDACs ints
  where
    zip :: (I.DAC a) => a -> Int -> Ivory eff ()
    zip dac i = do
        let ix = fromIntegral i
        let ao = E.aoutputs getAOutputs ! ix
        v <- deref $ ao ~> E.value
        I.setReduced dac v

forceSync :: (KnownNat n) => AOutputs n -> Ivory eff ()
forceSync AOutputs{..} =
    arrayMap \ix -> store (E.aoutputs getAOutputs ! ix ~> E.synced) false

sync :: (KnownNat n) => AOutputs n -> Ivory (ProcEffects s ()) ()
sync AOutputs{..} = do
    shouldInit' <- deref shouldInit
    when (iNot shouldInit') do
        i <- deref current
        let d = E.aoutputs getAOutputs ! toIx i
        synced' <- deref $ d ~> E.synced
        when (iNot synced') do
            msg <- E.message getAOutputs (i .% n)
            transmit msg
            store (d ~> E.synced) true
        store current $ i + 1

onInit ::
    (KnownNat l, KnownNat n) =>
    AOutputs n ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onInit AOutputs{..} buff size =
    when (size >=? 1 + n) do
        offset <- local $ ival 1
        let aos = E.aoutputs getAOutputs
        arrayMap \ix -> do
            offset' <- deref offset
            let ao = aos ! ix
            v <- unpack buff offset' :: Ivory eff Uint8
            store (ao ~> E.value) (safeCast v / 255)
            store offset $ offset' + 1
        store shouldInit false

onAo ::
    (KnownNat n, KnownNat l) =>
    AOutputs n ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory eff ()
onAo AOutputs{..} buff size = do
    when (size >=? 2) do
        shouldInit' <- deref shouldInit
        when (iNot shouldInit') do
            index <- deref $ buff ! 1
            when (index >=? 1 .&& index <=? n) do
                let index' = index - 1
                value <- deref $ buff ! 2
                E.set getAOutputs index' (safeCast value / 255)

