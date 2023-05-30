{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
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
import           Support.Device.GD32F3x0.DMA
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.System
import           Support.Device.GD32F3x0.Timer



data NeoPixel = NeoPixel
    { pwmTimer   :: Timer
    , pwmChannel :: TIMER_CHANNEL
    , dmaChannel :: DMA_CHANNEL
    , dmaParams  :: Record DMA_PARAM_STRUCT
    }

mkNeoPixel :: MonadWriter Context m
           => (Uint32 -> Uint32 -> m Timer)
           -> TIMER_CHANNEL
           -> DMA_CHANNEL
           -> Port
           -> m NeoPixel
mkNeoPixel timer' pwmChannel dmaChannel port = do
    pwmTimer <- timer' system_core_clock 100

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
            configTimerOutputMode         t pwmChannel timer_oc_mode_high
            configPrimaryOutput           t true
            enableTimerDMA                t timer_dma_upd
            configChannelOutputShadow     t pwmChannel timer_oc_shadow_enable
            enableTimer                   t

    addInit $ initPort port
    addInit initNeoPixel'

    pure NeoPixel { pwmTimer, pwmChannel, dmaChannel, dmaParams }



instance I.NeoPixel NeoPixel where
    transmitPixels NeoPixel{..} = undefined
        -- runFrame $ \frame -> do
        --     deinitDMA                   dmaChannel
        --     store (dmaParams ~> memory_addr) =<< castArrayToUint32 (toCArray frame)
        --     store (dmaParams ~> number) $ safeCast n
        --     initDMA                     dmaChannel dmaParams
        --     enableCirculationDMA        dmaChannel
        --     disableMemoryToMemoryDMA    dmaChannel
        --     enableChannelDMA            dmaChannel
