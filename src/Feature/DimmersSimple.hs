module Feature.DimmersSimple where

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
import Data.Value
import Endpoint.DimmersSimple qualified as D
import GHC.TypeNats
import Interface.MCU
import Interface.PWM qualified as I
import Ivory.Language
import Ivory.Stdlib

data DimmersSimple n = forall p. (I.PWM p) => DimmersSimple
    { n :: Uint8
    , getDimmers :: D.DimmersSimple n
    , getPWMs :: List n p
    , shouldInit :: Value IBool
    , current :: Index Uint8
    , transmit ::
        forall l.
        (KnownNat l) =>
        Buffer l Uint8 ->
        forall s t.
        Ivory (ProcEffects s t) ()
    }

mkDimmers ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , T.Transport t
    , I.PWM o
    , KnownNat n
    ) =>
    List n (p -> Uint32 -> Uint32 -> m o) ->
    Uint32 ->
    Uint8 ->
    t ->
    m (DimmersSimple n)
mkDimmers pwms period offset transport = do
    mcu <- asks D.mcu
    shouldInit <- asks D.shouldInit
    os <- mapM (\pwm -> pwm (peripherals mcu) 1_000_000 period) pwms
    let n = length os
    getDimmers <- D.mkDimmers "dimmers" offset
    current <- index "current_dimmer"

    let dimmers =
            DimmersSimple
                { n = fromIntegral n
                , getDimmers
                , getPWMs = os
                , shouldInit
                , current
                , transmit = T.transmitBuffer transport
                }

    addSync "dimmers" $ forceSync dimmers
    addTask $ yeld "dimmers_sync" $ sync dimmers

    pure dimmers

forceSync :: (KnownNat n) => DimmersSimple n -> Ivory eff ()
forceSync DimmersSimple{..} =
    arrayMap \ix -> store (D.dimmers getDimmers ! ix ~> D.synced) false

sync :: (KnownNat n) => DimmersSimple n -> Ivory (ProcEffects s ()) ()
sync DimmersSimple{..} = do
    shouldInit' <- deref shouldInit
    when (iNot shouldInit') do
        i <- deref current
        let d = D.dimmers getDimmers ! toIx i
        synced' <- deref $ d ~> D.synced
        when (iNot synced') do
            msg <- D.message getDimmers $ i .% n
            transmit msg
            store (d ~> D.synced) true
        store current $ i + 1

onDo ::
    (KnownNat n, KnownNat l) =>
    DimmersSimple n ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory eff ()
onDo DimmersSimple{..} buff size = do
    when (size >=? 3) do
        shouldInit' <- deref shouldInit
        when (iNot shouldInit') do
            index <- deref $ buff ! 1
            when (index >=? 1 .&& index <=? n) do
                let index' = index - 1
                value' <- deref $ buff ! 2
                ifte_
                    (value' ==? 0)
                    do onOff getDimmers index'
                    do onOn getDimmers index'

onDim ::
    (KnownNat n, KnownNat l) =>
    DimmersSimple n ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory eff ()
onDim DimmersSimple{..} buff size = do
    when (size >=? 3) do
        shouldInit' <- deref shouldInit
        when (iNot shouldInit') do
            index <- deref $ buff ! 1
            when (index >=? 1 .&& index <=? n) do
                let index' = index - 1
                action <- deref $ buff ! 2
                cond_
                    [ action ==? 0 ==> onOff getDimmers index'
                    , action ==? 1 ==> onOn  getDimmers index'
                    , action ==? 2 ==> onSet getDimmers index' buff size
                    , action ==? 3 ==> onSet getDimmers index' buff size
                    ]

onInit ::
    (KnownNat n, KnownNat l) =>
    DimmersSimple n ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onInit DimmersSimple{..} buff size =
    when (size >=? 1 + n) do
        offset <- local $ ival 1
        let ds = D.dimmers getDimmers
        arrayMap \ix -> do
            offset' <- deref offset
            let d = ds ! ix
            v <- unpack buff offset' :: Ivory eff Uint8
            store (d ~> D.value) (safeCast v / 255)
            store offset $ offset' + 1
        store shouldInit false


onOn :: (KnownNat n) => D.DimmersSimple n -> Uint8 -> Ivory eff ()
onOn = D.on 

onOff :: (KnownNat n) => D.DimmersSimple n -> Uint8 -> Ivory eff ()
onOff = D.off

onSet ::
    (KnownNat n, KnownNat l) =>
    D.DimmersSimple n ->
    Uint8 ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory eff ()
onSet dimmers index buff size =
    when (size >=? 4) do
        brightness <- unpack buff 3 :: Ivory eff Uint8
        D.set dimmers index (safeCast brightness / 255) 

