{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Device.GD32F3x0.NeoPixel where

import           Control.Monad.Writer
import           Core.Context
import           Core.Handler
import           Data.NeoPixel.Buffer.PWM
import           Data.Record
import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.Timer
import           GHC.TypeNats
import qualified Interface.NeoPixel             as I
import qualified Interface.PWM                  as I
import           Ivory.Language
import           Ivory.Support
import           Support.Cast
import           Support.Device.GD32F3x0.DMA
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.System
import           Support.Device.GD32F3x0.Timer
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.Misc



pwmPeriod :: Num a => a
pwmPeriod = 101



data NeoPixelPWM = NeoPixelPWM
    { pwmTimer   :: Timer
    , pwmChannel :: TIMER_CHANNEL
    , dmaChannel :: DMA_CHANNEL
    , dmaIRQn    :: IRQn
    , dmaParams  :: Record DMA_PARAM_STRUCT
    }

mkNeoPixelPWM :: MonadWriter Context m
              => (Uint32 -> Uint32 -> m Timer)
              -> TIMER_CHANNEL
              -> DMA_CHANNEL
              -> IRQn
              -> Port
              -> m NeoPixelPWM
mkNeoPixelPWM timer' pwmChannel dmaChannel dmaIRQn port = do
    pwmTimer <- timer' system_core_clock pwmPeriod

    let dmaInit = dmaParam [ direction    .= ival dma_memory_to_peripheral
                           , memory_inc   .= ival dma_memory_increase_enable
                           , memory_width .= ival dma_memory_width_8bit
                           , periph_inc   .= ival dma_periph_increase_disable
                           , periph_width .= ival dma_peripheral_width_16bit
                           , priority     .= ival dma_priority_ultra_high
                           ]
    dmaParams <- record (symbol dmaChannel <> "_dma_param") dmaInit

    let initNeoPixel' :: Def ('[] :-> ())
        initNeoPixel' = proc (show port <> "_pwm_init") $ body $ do
            enablePeriphClock             rcu_dma
            let t = timer pwmTimer
            store (dmaParams ~> periph_addr) =<< ch0cv t
            initChannelOcTimer            t pwmChannel =<< local (istruct timerOcDefaultParam)
            configChannelOutputPulseValue t pwmChannel 0
            configTimerOutputMode         t pwmChannel timer_oc_mode_pwm0
            configPrimaryOutput           t true
            enableTimerDMA                t timer_dma_upd
            configChannelOutputShadow     t pwmChannel timer_oc_shadow_enable
            enableTimer                   t
            enableIrqNvic                 dmaIRQn 1 0

    addInit $ initPort port
    addInit initNeoPixel'

    pure NeoPixelPWM { pwmTimer, pwmChannel, dmaChannel, dmaIRQn, dmaParams }



instance I.NeoPixel NeoPixelPWM NeoPixelBufferPWM where
    neoPixelBuffer _ id = neoPixelBufferPWM id pwmPeriod

    transmitPixels NeoPixelPWM{..} NeoPixelBufferPWM{..} =
        runFrame $ \frame -> do
            let frame' = addrOf frame
            deinitDMA                   dmaChannel
            store (dmaParams ~> memory_addr) =<< castArrayUint8ToUint32 (toCArray frame')
            store (dmaParams ~> number) $ arrayLen frame'
            initDMA                     dmaChannel dmaParams
            disableCirculationDMA       dmaChannel
            disableMemoryToMemoryDMA    dmaChannel
            clearInterruptFlagDMA       dmaChannel dma_int_flag_g
            enableInterruptDMA          dmaChannel dma_int_ftf
            enableChannelDMA            dmaChannel
