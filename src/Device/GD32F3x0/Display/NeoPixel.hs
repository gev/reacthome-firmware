{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Device.GD32F3x0.Display.NeoPixel where

import           Control.Monad.State               (MonadState)
import           Core.Context
import           Core.Handler
import           Core.Task
import           Data.Display.FrameBuffer.NeoPixel
import           Data.Index
import           Data.Record
import           Data.Value
import           Device.GD32F3x0.GPIO.Port
import           Device.GD32F3x0.Timer
import           GHC.TypeNats
import           Interface.Display
import qualified Interface.Timer                   as I
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Support
import           Support.Cast
import           Support.Device.GD32F3x0.DMA
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.System
import           Support.Device.GD32F3x0.Timer




pwmPeriod :: Num a => a
pwmPeriod = 101



data NeoPixel = NeoPixel
    { pwmTimer   :: Timer
    , pwmChannel :: TIMER_CHANNEL
    , pwmPort    :: Port
    , dmaChannel :: DMA_CHANNEL
    , dmaIRQn    :: IRQn
    , dmaParams  :: Record DMA_PARAM_STRUCT
    , buff'      :: FrameBufferNeoPixel Uint8
    , offset     :: Index Uint16
    }

mkNeoPixelPWM :: (MonadState Context m)
              => (Uint32 -> Uint32 -> m Timer)
              -> TIMER_CHANNEL
              -> DMA_CHANNEL
              -> IRQn
              -> (forall eff. TIMER_PERIPH -> Ivory eff Uint32)
              -> (GPIO_PUPD -> Port)
              -> m NeoPixel
mkNeoPixelPWM timer' pwmChannel dmaChannel dmaIRQn chxcv pwmPort' = do
    buff'        <- neoPixelBuffer (symbol dmaChannel) pwmPeriod
    let pwmPort   = pwmPort' gpio_pupd_none
    let dmaInit   = dmaParam [ direction    .= ival dma_memory_to_peripheral
                             , memory_inc   .= ival dma_memory_increase_enable
                             , memory_width .= ival dma_memory_width_8bit
                             , periph_inc   .= ival dma_periph_increase_disable
                             , periph_width .= ival dma_peripheral_width_16bit
                             , priority     .= ival dma_priority_low
                             , number       .= ival (arrayLen $ buff buff')
                             ]
    pwmTimer     <- timer' system_core_clock pwmPeriod
    dmaParams    <- record ("dma_param" <> symbol dmaChannel) dmaInit
    offset       <- index $ "neopixel_offset" <> symbol dmaChannel

    initPort pwmPort

    addInit (show pwmPort <> "_pwm") $ do
            enablePeriphClock             rcu_dma
            let t = timer pwmTimer
            store (dmaParams ~> periph_addr) =<< chxcv t
            initChannelOcTimer            t pwmChannel =<< local (istruct timerOcDefaultParam)
            configChannelOutputPulseValue t pwmChannel 0
            configTimerOutputMode         t pwmChannel timer_oc_mode_pwm0
            configChannelOutputShadow     t pwmChannel timer_oc_shadow_enable
            configPrimaryOutput           t true
            enableTimerDMA                t timer_dma_upd
            enableTimer                   t
            enableIrqNvic                 dmaIRQn 1 1
            disableCirculationDMA         dmaChannel
            enableInterruptDMA            dmaChannel dma_int_ftf

    pure NeoPixel { pwmTimer, pwmChannel, dmaIRQn, pwmPort, dmaChannel, dmaParams, buff', offset }



instance KnownNat n => Handler (Render n) NeoPixel where
  addHandler (Render npx@NeoPixel{..} frameRate frame render) = do

    addBody (makeIRQHandlerName dmaIRQn) (handleDMA npx frame)

    addInit ("neopixel_init" <> symbol dmaChannel) $ do
        render
        store offset 0
        transmitFrameBuffer npx frame

    addTask $ delay (1000 `iDiv` frameRate)
                    ("neo_pixel_" <> show pwmPort) $ do
                        shouldUpdate <- render
                        when shouldUpdate $ do
                            store offset 0
                            transmitFrameBuffer npx frame



handleDMA :: KnownNat n => NeoPixel -> Values n Uint8 -> Ivory eff ()
handleDMA npx@NeoPixel{..} frame = do
    f <- getInterruptFlagDMA dmaChannel dma_int_flag_ftf
    when f $ do
        clearInterruptFlagDMA dmaChannel dma_int_flag_g
        offset' <- deref offset
        when (offset' <? arrayLen frame) $
            transmitFrameBuffer npx frame



transmitFrameBuffer :: KnownNat n => NeoPixel -> Values n Uint8 ->Ivory eff ()
transmitFrameBuffer NeoPixel{..} frame = do
    offset' <- deref offset
    p <- deref (frame ! toIx offset')
    writeByte buff' p
    store (dmaParams ~> memory_addr) =<< castArrayUint8ToUint32 (toCArray $ buff buff')
    I.resetCounter   pwmTimer
    initDMA          dmaChannel dmaParams
    enableChannelDMA dmaChannel
    store offset $ offset' + 1



instance Display NeoPixel
