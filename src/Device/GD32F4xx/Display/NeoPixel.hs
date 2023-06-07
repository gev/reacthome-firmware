{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Device.GD32F4xx.Display.NeoPixel where

import           Control.Monad.Writer                  (MonadWriter)
import           Core.Context
import           Core.Handler
import           Core.Task
import           Data.Display.FrameBuffer.NeoPixel.PWM
import           Data.Record
import           Data.Value
import           Device.GD32F4xx.GPIO
import           Device.GD32F4xx.Timer
import qualified Interface.Display                     as I
import qualified Interface.Timer                       as I
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Support
import           Support.Cast
import           Support.Device.GD32F4xx.DMA
import           Support.Device.GD32F4xx.IRQ
import           Support.Device.GD32F4xx.Misc
import           Support.Device.GD32F4xx.RCU
import           Support.Device.GD32F4xx.System
import           Support.Device.GD32F4xx.Timer



pwmPeriod :: Num a => a
pwmPeriod = 101



data NeoPixelPWM = NeoPixelPWM
    { pwmTimer   :: Timer
    , pwmChannel :: TIMER_CHANNEL
    , pwmPort    :: Port
    , dmaRcu     :: RCU_PERIPH
    , dmaPer     :: DMA_PERIPH
    , dmaChannel :: DMA_CHANNEL
    , dmaSubPer  :: DMA_SUBPERIPH
    , dmaParams  :: Record DMA_SINGLE_PARAM_STRUCT
    }

mkNeoPixelPWM :: MonadWriter Context m
              => (Uint32 -> Uint32 -> m Timer)
              -> TIMER_CHANNEL
              -> RCU_PERIPH
              -> DMA_PERIPH
              -> DMA_CHANNEL
              -> DMA_SUBPERIPH
              -> Port
              -> m NeoPixelPWM
mkNeoPixelPWM timer' pwmChannel dmaRcu dmaPer dmaChannel dmaSubPer pwmPort = do
    pwmTimer     <- timer' system_core_clock pwmPeriod
    let dmaInit   = dmaParam [ direction            .= ival dma_memory_to_periph
                             , memory_inc           .= ival dma_memory_increase_enable
                             , periph_memory_width  .= ival dma_periph_width_8bit
                             , circular_mode        .= ival dma_circular_mode_disable
                             , periph_inc           .= ival dma_periph_increase_disable
                             , priority             .= ival dma_priority_ultra_high
                             ]
    dmaParams    <- record (symbol dmaChannel <> "_dma_param") dmaInit
    frameRequest <- value  (symbol dmaChannel <> "_frame_request" ) true

    let initNeoPixel' :: Def ('[] :-> ())
        initNeoPixel' = proc (show pwmPort <> "_pwm_init") $ body $ do
            enablePeriphClock             dmaRcu
            let t = timer pwmTimer
            store (dmaParams ~> periph_addr) =<< ch0cv t
            initChannelOcTimer            t pwmChannel =<< local (istruct timerOcDefaultParam)
            configChannelOutputPulseValue t pwmChannel 0
            configTimerOutputMode         t pwmChannel timer_oc_mode_pwm0
            configChannelOutputShadow     t pwmChannel timer_oc_shadow_enable
            configPrimaryOutput           t true
            enableTimerDMA                t timer_dma_upd
            enableTimer                   t

    addInit $ initPort pwmPort
    addInit initNeoPixel'

    pure NeoPixelPWM { pwmTimer, pwmChannel, pwmPort, dmaRcu, dmaPer, dmaChannel, dmaSubPer, dmaParams }



instance Handler I.Render NeoPixelPWM where
  addHandler (I.Render NeoPixelPWM{..} frameRate render) =
    addTask $ delay (1000 `iDiv` frameRate)
                    (show pwmPort <> "neo_pixel")
                    render



instance I.Display NeoPixelPWM FrameBufferNeoPixelPWM where
    frameBuffer _ = neoPixelBufferPWM pwmPeriod

    transmitFrameBuffer NeoPixelPWM{..} FrameBufferNeoPixelPWM{..} =
        runFrame $ \frame -> do
            let frame' = addrOf frame
            store (dmaParams ~> memory0_addr) =<< castArrayUint8ToUint32 (toCArray frame')
            store (dmaParams ~> number) $ arrayLen frame'
            deinitDMA                       dmaPer dmaChannel
            initSingleDMA                   dmaPer dmaChannel dmaParams
            disableCirculationDMA           dmaPer dmaChannel
            selectChannelSubperipheralDMA   dmaPer dmaChannel dmaSubPer
            I.resetCounter                  pwmTimer
            enableChannelDMA                dmaPer dmaChannel
