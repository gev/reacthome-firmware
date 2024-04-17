{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
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
    , buff       :: FrameBufferNeoPixel Uint8
    , offset     :: Index Uint16
    }

mkNeoPixelPWM :: MonadState Context m
              => (Uint32 -> Uint32 -> m Timer)
              -> TIMER_CHANNEL
              -> DMA_CHANNEL
              -> IRQn
              -> (GPIO_PUPD -> Port)
              -> m NeoPixel
mkNeoPixelPWM timer' pwmChannel dmaChannel dmaIRQn pwmPort' = do
    let pwmPort   = pwmPort' gpio_pupd_none
    let dmaInit   = dmaParam [ direction    .= ival dma_memory_to_peripheral
                             , memory_inc   .= ival dma_memory_increase_enable
                             , memory_width .= ival dma_memory_width_8bit
                             , periph_inc   .= ival dma_periph_increase_disable
                             , periph_width .= ival dma_peripheral_width_16bit
                             , priority     .= ival dma_priority_ultra_high
                             ]
    pwmTimer     <- timer' system_core_clock pwmPeriod
    dmaParams    <- record (symbol dmaChannel <> "_dma_param") dmaInit
    buff         <- neoPixelBuffer pwmPeriod 1
    offset       <- index "neopixel"

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
            enableIrqNvic       dmaIRQn 0 0


    pure NeoPixel { pwmTimer, pwmChannel, dmaIRQn, pwmPort, dmaChannel, dmaParams, buff, offset }



instance Handler Render NeoPixel where
  addHandler (Render npx@NeoPixel{..} frameRate runFrame render) = do
    addBody (makeIRQHandlerName dmaIRQn) (handleDMA npx runFrame)
    addTask $ delay (1000 `iDiv` frameRate)
                    ("neo_pixel_" <> show pwmPort) $ do
                        render
                        runFrame $ \frame -> do
                            writeByte buff 0 =<< deref (addrOf frame ! 0)
                            store offset 1
                            transmitFrameBuffer npx



handleDMA :: NeoPixel -> RunValues Uint8 -> Ivory ('Effects (Returns ()) b (Scope s)) ()
handleDMA npx@NeoPixel{..} runFrame = do
    f <- getInterruptFlagDMA dmaChannel dma_int_flag_ftf
    when f $ do
        clearInterruptFlagDMA dmaChannel dma_int_flag_g
        offset' <- deref offset
        runFrame $ \frame -> do
            let frame' = addrOf frame
            when (offset' <? arrayLen frame') $ do
                writeByte buff 0 =<< deref (frame' ! toIx offset')
                transmitFrameBuffer npx
                store offset $ offset' + 1



transmitFrameBuffer :: NeoPixel -> Ivory eff ()
transmitFrameBuffer NeoPixel{..} =
        runBuffer buff $ \b -> do
            let b' = addrOf b
            store (dmaParams ~> memory_addr) =<< castArrayUint8ToUint32 (toCArray b')
            store (dmaParams ~> number) $ arrayLen b'
            deinitDMA                     dmaChannel
            initDMA                       dmaChannel dmaParams
            disableCirculationDMA         dmaChannel
            I.resetCounter                pwmTimer
            enableInterruptDMA            dmaChannel dma_int_ftf
            enableChannelDMA              dmaChannel



instance Display NeoPixel
