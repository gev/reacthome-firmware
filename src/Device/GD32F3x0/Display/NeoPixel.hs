{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Device.GD32F3x0.Display.NeoPixel where

import           Control.Monad.Writer                  (MonadWriter)
import           Core.Context
import           Core.Handler
import           Core.Task
import           Data.Display.FrameBuffer.NeoPixel.PWM
import           Data.Record
import           Data.Value
import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.Timer
import qualified Interface.Display                     as I
import qualified Interface.Timer                       as I
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Support
import           Support.Cast
import           Support.Device.GD32F3x0.DMA
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.System
import           Support.Device.GD32F3x0.Timer



pwmPeriod :: Num a => a
pwmPeriod = 101



data NeoPixelPWM = NeoPixelPWM
    { pwmTimer   :: Timer
    , pwmChannel :: TIMER_CHANNEL
    , pwmPort    :: Port
    , dmaChannel :: DMA_CHANNEL
    , dmaParams  :: Record DMA_PARAM_STRUCT
    }

mkNeoPixelPWM :: MonadWriter Context m
              => (Uint32 -> Uint32 -> m Timer)
              -> TIMER_CHANNEL
              -> DMA_CHANNEL
              -> Port
              -> m NeoPixelPWM
mkNeoPixelPWM timer' pwmChannel dmaChannel pwmPort = do
    pwmTimer     <- timer' system_core_clock pwmPeriod
    let dmaInit   = dmaParam [ direction    .= ival dma_memory_to_peripheral
                             , memory_inc   .= ival dma_memory_increase_enable
                             , memory_width .= ival dma_memory_width_8bit
                             , periph_inc   .= ival dma_periph_increase_disable
                             , periph_width .= ival dma_peripheral_width_16bit
                             , priority     .= ival dma_priority_ultra_high
                             ]
    dmaParams    <- record (symbol dmaChannel <> "_dma_param") dmaInit
    frameRequest <- value  (symbol dmaChannel <> "_frame_request" ) true

    initPort pwmPort

    addInit (show pwmPort <> "_pwm") $ do
            enablePeriphClock             rcu_dma
            let t = timer pwmTimer
            store (dmaParams ~> periph_addr) =<< ch0cv t
            initChannelOcTimer            t pwmChannel =<< local (istruct timerOcDefaultParam)
            configChannelOutputPulseValue t pwmChannel 0
            configTimerOutputMode         t pwmChannel timer_oc_mode_pwm0
            configChannelOutputShadow     t pwmChannel timer_oc_shadow_enable
            configPrimaryOutput           t true
            enableTimerDMA                t timer_dma_upd
            enableTimer                   t

    pure NeoPixelPWM { pwmTimer, pwmChannel, pwmPort, dmaChannel, dmaParams }



instance Handler I.Render NeoPixelPWM where
  addHandler (I.Render NeoPixelPWM{..} frameRate render) =
    addTask $ delay (1000 `iDiv` frameRate)
                    (show pwmPort <> "neo_pixel")
                    render



instance I.Display NeoPixelPWM FrameBufferNeoPixelPWM Uint8 where
    frameBuffer _ = neoPixelBufferPWM pwmPeriod

    transmitFrameBuffer NeoPixelPWM{..} FrameBufferNeoPixelPWM{..} =
        runFrame $ \frame -> do
            let frame' = addrOf frame
            store (dmaParams ~> memory_addr) =<< castArrayUint8ToUint32 (toCArray frame')
            store (dmaParams ~> number) $ arrayLen frame'
            deinitDMA                   dmaChannel
            initDMA                     dmaChannel dmaParams
            disableCirculationDMA       dmaChannel
            disableMemoryToMemoryDMA    dmaChannel
            I.resetCounter              pwmTimer
            enableChannelDMA            dmaChannel
