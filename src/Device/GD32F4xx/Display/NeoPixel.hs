{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Device.GD32F4xx.Display.NeoPixel where

import           Control.Monad.State               (MonadState)
import           Core.Context
import           Core.Handler
import           Core.Task
import           Data.Display.FrameBuffer.NeoPixel
import           Data.Index
import           Data.Record
import           Data.Value
import           Device.GD32F4xx.GPIO.Port
import           Device.GD32F4xx.Timer
import           GHC.TypeNats
import           Interface.Display
import qualified Interface.Timer                   as I
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Support
import           Support.Cast
import           Support.Device.GD32F4xx.DMA
import           Support.Device.GD32F4xx.GPIO
import           Support.Device.GD32F4xx.IRQ
import           Support.Device.GD32F4xx.Misc
import           Support.Device.GD32F4xx.RCU
import           Support.Device.GD32F4xx.System
import           Support.Device.GD32F4xx.Timer



pwmPeriod :: Num a => a
pwmPeriod = 120



data NeoPixel (np :: Nat) = NeoPixel
    { pwmTimer   :: Timer
    , pwmChannel :: TIMER_CHANNEL
    , pwmPort    :: Port
    , dmaRcu     :: RCU_PERIPH
    , dmaPer     :: DMA_PERIPH
    , dmaChannel :: DMA_CHANNEL
    , dmaSubPer  :: DMA_SUBPERIPH
    , dmaIRQn    :: IRQn
    , dmaParams  :: Record DMA_SINGLE_PARAM_STRUCT
    , buff       :: FrameBufferNeoPixel np Uint16
    , offset     :: Index Uint16
    }



mkNeoPixelPWM :: (MonadState Context m, KnownNat (BufferSize np))
              => (Uint32 -> Uint32 -> m Timer)
              -> TIMER_CHANNEL
              -> RCU_PERIPH
              -> DMA_PERIPH
              -> DMA_CHANNEL
              -> DMA_SUBPERIPH
              -> IRQn
              -> (forall eff. TIMER_PERIPH -> Ivory eff Uint32)
              -> (GPIO_PUPD -> Port)
              -> m (NeoPixel np)
mkNeoPixelPWM timer' pwmChannel dmaRcu dmaPer dmaChannel dmaSubPer dmaIRQn selChPWM pwmPort' = do
    let pwmPort   = pwmPort' gpio_pupd_none
    let dmaInit   = dmaParam [ direction           .= ival dma_memory_to_periph
                             , memory_inc          .= ival dma_memory_increase_enable
                             , periph_memory_width .= ival dma_periph_width_16bit
                             , circular_mode       .= ival dma_circular_mode_disable
                             , periph_inc          .= ival dma_periph_increase_disable
                             , priority            .= ival dma_priority_ultra_high
                             ]
    pwmTimer     <- timer' 100_000_000 pwmPeriod
    dmaParams    <- record ("dma_param" <> symbol dmaPer <> symbol dmaChannel) dmaInit
    buff         <- neoPixelBuffer (symbol dmaPer <> symbol dmaChannel) pwmPeriod
    offset       <- index $ "neopixel_offset" <> symbol dmaPer <> symbol dmaChannel

    initPort pwmPort

    addInit (show pwmPort <> "_pwm") $ do
            enablePeriphClock             dmaRcu
            let t = timer pwmTimer
            store (dmaParams ~> periph_addr) =<< selChPWM t
            initChannelOcTimer            t pwmChannel =<< local (istruct timerOcDefaultParam)
            configChannelOutputPulseValue t pwmChannel 0
            configTimerOutputMode         t pwmChannel timer_oc_mode_pwm0
            configChannelOutputShadow     t pwmChannel timer_oc_shadow_enable
            configPrimaryOutput           t true
            enableTimerDMA                t timer_dma_upd
            enableTimer                   t
            enableIrqNvic                 dmaIRQn 0 0

    pure NeoPixel { pwmTimer, pwmChannel, pwmPort, dmaRcu, dmaPer, dmaChannel, dmaSubPer, dmaIRQn, dmaParams, buff, offset }



instance (KnownNat n, KnownNat np, KnownNat (BufferSize np)) => Handler (Render n) (NeoPixel np) where
  addHandler (Render npx@NeoPixel{..} frameRate frame render) = do
    addModule $ makeIRQHandler dmaIRQn $ handleDMA npx frame
    addTask $ delay (1000 `iDiv` frameRate)
                    ("neo_pixel_" <> show pwmPort) $ do
                        render
                        store offset 0
                        arrayMap $ \(ix :: Ix np) -> do
                            offset' <- deref offset
                            writeByte buff (fromIx ix) =<< deref (frame ! toIx offset')
                            store offset $ offset' + 1
                        transmitFrameBuffer npx



handleDMA :: forall n np s b. (KnownNat n, KnownNat np, KnownNat (BufferSize np))
          => NeoPixel np -> Values n Uint8 -> Ivory ('Effects (Returns ()) b (Scope s)) ()
handleDMA npx@NeoPixel{..} frame = do
    f <- getInterruptFlagDMA  dmaPer dmaChannel dma_int_flag_ftf
    when f $ do
        clearInterruptFlagDMA dmaPer dmaChannel dma_int_flag_ftf
        offset' <- deref offset
        when (offset' <? arrayLen frame) $ do
            arrayMap $ \(ix :: Ix np) -> do
                offset' <- deref offset
                writeByte buff (fromIx ix) =<< deref (frame ! toIx offset')
                store offset $ offset' + 1
            transmitFrameBuffer npx



transmitFrameBuffer :: KnownNat (BufferSize np) =>NeoPixel np -> Ivory eff ()
transmitFrameBuffer NeoPixel{..} = do
    let b = buffer buff
    store (dmaParams ~> memory0_addr) =<< castArrayUint16ToUint32 (toCArray b)
    store (dmaParams ~> number) $ arrayLen b
    deinitDMA                     dmaPer dmaChannel
    initSingleDMA                 dmaPer dmaChannel dmaParams
    disableCirculationDMA         dmaPer dmaChannel
    selectChannelSubperipheralDMA dmaPer dmaChannel dmaSubPer
    I.resetCounter                pwmTimer
    enableInterruptDMA            dmaPer dmaChannel dma_chxctl_ftfie
    enableChannelDMA              dmaPer dmaChannel



instance Display (NeoPixel np)
