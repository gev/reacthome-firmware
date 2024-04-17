{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.Smart.Top.Vibro where

import           Control.Monad.Reader  (MonadReader, asks)
import           Control.Monad.State   (MonadState)
import           Core.Actions
import           Core.Context
import qualified Core.Domain           as D
import           Core.Task             (yeld)
import           Core.Transport
import           Data.Buffer
import           Data.Serialize        (unpack)
import           Data.Value
import           Endpoint.DInputs      (DInputs (runDInputs), state)
import           Feature.Relays        (Relays (shouldInit))
import           GHC.TypeNats
import           Interface.GPIO.Output (Output, reset, set)
import           Interface.GPIO.Port   (Pull, pullNone)
import           Interface.MCU         (MCU (peripherals, systemClock))
import           Interface.SystemClock (SystemClock, getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib



data Vibro = forall o t. (Output o, LazyTransport t) => Vibro
    { dinputs     :: DInputs
    , output      :: o
    , clock       :: SystemClock
    , volume      :: Value Uint8
    , isVibrating :: Value IBool
    , prevState   :: Values 12 IBool
    , t           :: Value Uint32
    , transport   :: t
    }



vibro :: (MonadState Context m, MonadReader (D.Domain p c) m, Output o, LazyTransport t, Pull p d)
      => (p -> d -> m o) -> DInputs -> t -> m Vibro
vibro output' dinputs transport = do
    mcu             <- asks D.mcu
    let clock        = systemClock mcu
    let peripherals' = peripherals mcu
    output          <- output' peripherals' $ pullNone peripherals'
    volume          <- value "vibro_volume" 100
    isVibrating     <- value "is_vibrating" false
    prevState       <- values "prev_state" $ replicate 12 false
    t               <- value "t" 0

    let vibro = Vibro { dinputs, output, clock
                      , volume, t, isVibrating, prevState
                      , transport
                      }

    addTask $ yeld "vibro" $ vibroTask vibro

    pure vibro



vibroTask :: Vibro -> Ivory (ProcEffects s t) ()
vibroTask v@Vibro{..} = do
    isVibrating' <- deref isVibrating
    ifte_ isVibrating'
        (do
            t0 <- deref t
            t1 <- getSystemTime clock
            volume' <- deref volume
            when (t1 - t0 >? safeCast volume') $ stopVibrate v

        )
        (runDInputs dinputs $ \di -> do
            shouldVibrate <- local $ ival false
            arrayMap $ \ix -> do
                shouldVibrate' <- deref shouldVibrate
                state' <- deref $ addrOf di ! ix ~> state
                prevState' <- deref $ prevState ! toIx ix
                store shouldVibrate $ shouldVibrate' .|| (state' .&& iNot prevState')
                store (prevState ! toIx ix) state'
            shouldVibrate' <- deref shouldVibrate
            when shouldVibrate' $ startVibrate v
        )



startVibrate :: Vibro -> Ivory eff ()
startVibrate Vibro{..} = do
    store isVibrating true
    store t =<< getSystemTime clock
    set output



stopVibrate :: Vibro -> Ivory eff ()
stopVibrate Vibro{..} = do
    store isVibrating false
    reset output



onVibro :: (KnownNat n) => Vibro -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onVibro v@Vibro{..} buffer size =
    when (size ==? 2) $ do
        volume' <- unpack buffer 1
        store volume volume'
        startVibrate v
        lazyTransmit transport 2 $ \transmit -> do
            transmit actionVibro
            transmit volume'



onInitVibro :: (KnownNat n) => Vibro -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) IBool
onInitVibro v@Vibro{..} buffer size = do
    ifte (size >=? 2)
         (do
            store volume =<< deref (buffer ! 1)
            pure true
         )
         (  pure false
         )
