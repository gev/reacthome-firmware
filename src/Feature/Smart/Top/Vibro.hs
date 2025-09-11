{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Feature.Smart.Top.Vibro where

import           Control.Monad.Reader  (MonadReader, asks)
import           Control.Monad.State   (MonadState)
import           Core.Actions
import           Core.Context
import qualified Core.Domain           as D
import           Core.Task             (delay, yeld)
import           Core.Transport
import           Data.Buffer
import           Data.Serialize        (unpack)
import           Data.Value
import           Endpoint.DInputs      (DInputs (dinputs), state)
import           Feature.Relays        (Relays (shouldInit))
import           GHC.TypeNats
import           Interface.Flash       as F
import           Interface.GPIO.Output (Output, reset, set)
import           Interface.GPIO.Port   (Pull, pullNone)
import           Interface.MCU
import           Interface.SystemClock (SystemClock, getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib
import           Util.CRC16



data Vibro n = forall o t f. (Output o, LazyTransport t, Flash f) => Vibro
    { getDInputs  :: DInputs n
    , output      :: o
    , clock       :: SystemClock
    , volume      :: Value Uint8
    , isVibrating :: Value IBool
    , prevState   :: Values n IBool
    , t           :: Value Uint32
    , transport   :: t
    , etc         :: f
    , synced      :: Value IBool
    , before      :: forall eff. Ivory eff ()
    , after       :: forall eff. Ivory eff ()
    }



vibro :: ( MonadState Context m
         , MonadReader (D.Domain p c) m
         , Output o
         , Pull p d
         , Flash f
         , LazyTransport t
         , KnownNat n
         )
      => (p -> d -> m o) -> DInputs n -> t -> f -> (forall eff. Ivory eff ()) -> (forall eff. Ivory eff ()) -> m (Vibro n)
vibro output' getDInputs transport etc before after = do
    mcu             <- asks D.mcu
    let clock        = systemClock mcu
    let peripherals' = peripherals mcu
    output          <- output' peripherals' $ pullNone peripherals'
    volume          <- value "vibro_volume" 100
    isVibrating     <- value "is_vibrating" false
    prevState       <- values "prev_state" $ replicate 12 false
    t               <- value "t" 0
    synced          <- value "synced" true

    let vibro = Vibro { getDInputs, output, clock
                      , volume, t, isVibrating, prevState
                      , transport, etc
                      , synced
                      , before, after
                      }

    addTask $ yeld        "vibro"      $ vibroTask vibro
    addTask $ delay 1_000 "sync_vibro" $ syncVibro vibro

    addInit "load_vibro" $ loadVibro vibro

    pure vibro



syncVibro :: Vibro n -> Ivory (ProcEffects s t) ()
syncVibro Vibro{..} = do
    synced' <- deref synced
    when (iNot synced') $ do
        erasePage etc 0
        crc     <- local $ istruct initCRC16
        volume' <- deref volume
        updateCRC16 crc volume'
        F.write etc 0 $ safeCast volume'
        F.write etc 4 . safeCast =<< deref (crc ~> msb)
        F.write etc 8 . safeCast =<< deref (crc ~> lsb)
        store synced true



loadVibro :: Vibro n -> Ivory (ProcEffects s t) ()
loadVibro Vibro{..} = do
    volume' <- castDefault <$> F.read etc 0
    msb'    <- F.read etc 4
    lsb'    <- F.read etc 8
    crc     <- local $ istruct initCRC16
    updateCRC16 crc volume'
    msb     <- safeCast <$> deref (crc ~> msb)
    lsb     <- safeCast <$> deref (crc ~> lsb)
    when (msb ==? msb' .&& lsb ==? lsb') $
        store volume volume'


vibroTask :: KnownNat n => Vibro n -> Ivory (ProcEffects s t) ()
vibroTask v@Vibro{..} = do
    isVibrating' <- deref isVibrating
    ifte_ isVibrating'
        (do
            t0 <- deref t
            t1 <- getSystemTime clock
            volume' <- deref volume
            when (t1 - t0 >? safeCast volume') $ stopVibrate v

        )
        (do
            shouldVibrate <- local $ ival false
            arrayMap $ \ix -> do
                shouldVibrate' <- deref shouldVibrate
                state' <- deref $ dinputs getDInputs ! ix ~> state
                prevState' <- deref $ prevState ! toIx ix
                store shouldVibrate $ shouldVibrate' .|| (state' .&& iNot prevState')
                store (prevState ! toIx ix) state'
            shouldVibrate' <- deref shouldVibrate
            when shouldVibrate' $ startVibrate v
        )



startVibrate :: Vibro n -> Ivory eff ()
startVibrate Vibro{..} = do
    store isVibrating true
    store t =<< getSystemTime clock
    before
    set output



stopVibrate :: Vibro n -> Ivory eff ()
stopVibrate Vibro{..} = do
    store isVibrating false
    reset output
    after



onVibro :: (KnownNat n, KnownNat l)
        => Vibro n -> Buffer l Uint8 -> Uint8
        -> Ivory (ProcEffects s t) ()
onVibro v@Vibro{..} buffer size =
    when (size ==? 2) $ do
        volume' <- unpack buffer 1
        store volume volume'
        store synced false
        startVibrate v
        lazyTransmit transport 2 $ \transmit -> do
            transmit actionVibro
            transmit volume'



onInitVibro :: (KnownNat n, KnownNat l)
            => Vibro n -> Buffer l Uint8 -> Uint8
            -> Ivory (ProcEffects s t) IBool
onInitVibro v@Vibro{..} buffer size = do
    ifte (size >=? 2)
         (do
            store volume =<< deref (buffer ! 1)
            pure true
         )
         (  pure false
         )


sendVibro Vibro{..} = do
    lazyTransmit transport 2 $ \transmit -> do
        transmit actionVibro
        transmit =<< deref volume
