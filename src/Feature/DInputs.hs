{-# HLINT ignore "Use for_" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Feature.DInputs where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Context
import qualified Core.Domain as D
import Core.Task
import qualified Core.Transport as T
import Data.Buffer
import Data.Fixed
import Data.Index
import Data.Record
import Data.Serialize
import Data.Value
import qualified Endpoint.DInputs as DI
import GHC.TypeNats
import Interface.GPIO.Input
import Interface.GPIO.Port
import Interface.MCU (MCU, peripherals, systemClock)
import Interface.SystemClock (SystemClock, getSystemTime)
import Ivory.Language
import Ivory.Language.Proxy
import Ivory.Stdlib

data DInputs n = forall i. (Input i) => DInputs
    { zero :: IBool
    , getDInputs :: DI.DInputs n
    , getInputs :: List n i
    , current :: Index Uint8
    , clock :: SystemClock
    , transmit ::
        forall l.
        (KnownNat l) =>
        Buffer l Uint8 ->
        forall s.
        Ivory (ProcEffects s ()) ()
    }

dinputs ::
    forall m n p c i d t.
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , T.Transport t
    , Input i
    , Pull p d
    , KnownNat n
    ) =>
    List n (p -> d -> m i) ->
    Bool ->
    t ->
    m (DInputs n)
dinputs inputs zero' transport = do
    mcu <- asks D.mcu
    let clock = systemClock mcu
    let peripherals' = peripherals mcu
    let pull = if zero' then pullUp else pullDown
    is <- mapM (($ pull peripherals') . ($ peripherals')) inputs
    getDInputs <- DI.mkDinputs "dinputs"
    current <- index "current_dinput"

    let dinputs =
            DInputs
                { zero = if zero' then true else false
                , getDInputs
                , getInputs = is
                , current
                , clock
                , transmit = T.transmitBuffer transport
                }

    addTask $ delay 10 "dinputs_manage" $ manageDInputs dinputs
    addTask $ yeld "dinputs_sync" $ syncDInputs dinputs

    addSync "dinputs" $ forceSyncDInputs dinputs

    pure dinputs

forceSyncDInputs :: (KnownNat n) => DInputs n -> Ivory eff ()
forceSyncDInputs DInputs{..} = do
    arrayMap \ix -> store ((DI.dinputs getDInputs ! ix) ~> DI.synced) false

manageDInputs :: (KnownNat n) => DInputs n -> Ivory eff ()
manageDInputs DInputs{..} = zipWithM_ zip getInputs ints
  where
    zip :: (Input i) => i -> Int -> Ivory eff ()
    zip input i = do
        let ix = fromIntegral i
        let di = DI.dinputs getDInputs ! ix
        manageDInput zero di input =<< getSystemTime clock

manageDInput ::
    (Input i) =>
    IBool ->
    Record DI.DInputStruct ->
    i ->
    Uint32 ->
    Ivory eff ()
manageDInput zero di input t = do
    state0 <- deref $ di ~> DI.state
    state1 <- (/=? zero) <$> get input
    when (state1 /=? state0) do
        store (di ~> DI.state) state1
        store (di ~> DI.timestamp) t
        store (di ~> DI.synced) false

syncDInputs :: (KnownNat n) => DInputs n -> Ivory (ProcEffects s ()) ()
syncDInputs dis@DInputs{..} = do
    i <- deref current
    syncDInput dis i
    store current $ i + 1

syncDInput :: (KnownNat n) => DInputs n -> Uint8 -> Ivory (ProcEffects s ()) ()
syncDInput DInputs{..} i = do
    let n = fromIntegral $ length getInputs
    let di = DI.dinputs getDInputs ! toIx i
    synced <- deref $ di ~> DI.synced
    when (iNot synced) do
        msg <- DI.message getDInputs (i .% n)
        transmit msg
        store (di ~> DI.synced) true
