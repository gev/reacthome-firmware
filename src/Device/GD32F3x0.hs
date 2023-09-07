{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}


module Device.GD32F3x0 where

import           Control.Monad.State
import           Core.Context
import           Device.GD32F3x0.Display.NeoPixel
import           Device.GD32F3x0.EXTI
import           Device.GD32F3x0.Flash
import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.GPIO.Input
import           Device.GD32F3x0.GPIO.Mode
import           Device.GD32F3x0.GPIO.OpenDrain
import           Device.GD32F3x0.GPIO.Output
import           Device.GD32F3x0.Mac              (makeMac)
import           Device.GD32F3x0.OneWire
import           Device.GD32F3x0.PWM
import           Device.GD32F3x0.SystemClock      as G
import           Device.GD32F3x0.SysTick
import           Device.GD32F3x0.Timer
import           Device.GD32F3x0.UART
import           Interface.Mac                    (Mac)
import           Interface.MCU
import           Interface.SystemClock            (SystemClock, systemClock)
import           Ivory.Language
import           Support.Device.GD32F3x0
import           Support.Device.GD32F3x0.DMA
import           Support.Device.GD32F3x0.EXTI
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.RCU      as R
import           Support.Device.GD32F3x0.SYSCFG
import           Support.Device.GD32F3x0.Timer
import           Support.Device.GD32F3x0.USART



type UARTW        = forall m. MonadState Context m => m UART
type InputW       = forall m. MonadState Context m => m Input
type OutputW      = forall m. MonadState Context m => m Output
type OpenDrainW   = forall m. MonadState Context m => m OpenDrain
type PWMW         = forall m. MonadState Context m => Uint32 -> Uint32 -> m PWM
type TimerW       = forall m. MonadState Context m => Uint32 -> Uint32 -> m Timer
type NeoPixelPWMW = forall m. MonadState Context m => m NeoPixelPWM
type EXTIW        = forall m. MonadState Context m => m EXTI
type OneWireW     = forall m. MonadState Context m => m OneWire


data GD32F3x0 = GD32F3x0
    { uart_0    :: UARTW
    , uart_1    :: UARTW

    , in_pa_0   :: InputW
    , in_pa_1   :: InputW
    , in_pa_2   :: InputW
    , in_pa_3   :: InputW
    , in_pa_4   :: InputW
    , in_pa_5   :: InputW
    , in_pa_6   :: InputW
    , in_pa_7   :: InputW
    , in_pa_8   :: InputW
    , in_pa_9   :: InputW
    , in_pa_10  :: InputW
    , in_pa_11  :: InputW
    , in_pa_12  :: InputW
    , in_pa_13  :: InputW
    , in_pa_14  :: InputW
    , in_pa_15  :: InputW

    , in_pb_0   :: InputW
    , in_pb_1   :: InputW
    , in_pb_2   :: InputW
    , in_pb_3   :: InputW
    , in_pb_4   :: InputW
    , in_pb_5   :: InputW
    , in_pb_6   :: InputW
    , in_pb_7   :: InputW
    , in_pb_8   :: InputW
    , in_pb_9   :: InputW
    , in_pb_10  :: InputW
    , in_pb_11  :: InputW
    , in_pb_12  :: InputW
    , in_pb_13  :: InputW
    , in_pb_14  :: InputW
    , in_pb_15  :: InputW

    , out_pa_0  :: OutputW
    , out_pa_1  :: OutputW
    , out_pa_2  :: OutputW
    , out_pa_3  :: OutputW
    , out_pa_4  :: OutputW
    , out_pa_5  :: OutputW
    , out_pa_6  :: OutputW
    , out_pa_7  :: OutputW
    , out_pa_8  :: OutputW
    , out_pa_9  :: OutputW
    , out_pa_10 :: OutputW
    , out_pa_11 :: OutputW
    , out_pa_12 :: OutputW
    , out_pa_13 :: OutputW
    , out_pa_14 :: OutputW
    , out_pa_15 :: OutputW

    , out_pb_0  :: OutputW
    , out_pb_1  :: OutputW
    , out_pb_2  :: OutputW
    , out_pb_3  :: OutputW
    , out_pb_4  :: OutputW
    , out_pb_5  :: OutputW
    , out_pb_6  :: OutputW
    , out_pb_7  :: OutputW
    , out_pb_8  :: OutputW
    , out_pb_9  :: OutputW
    , out_pb_10 :: OutputW
    , out_pb_11 :: OutputW
    , out_pb_12 :: OutputW
    , out_pb_13 :: OutputW
    , out_pb_14 :: OutputW
    , out_pb_15 :: OutputW

    , od_pa_8   :: OpenDrainW

    , tim_0     :: TimerW
    , tim_1     :: TimerW
    , tim_2     :: TimerW
    , tim_14    :: TimerW
    , tim_15    :: TimerW

    , pwm_0     :: PWMW
    , pwm_1     :: PWMW
    , pwm_2     :: PWMW
    , pwm_3     :: PWMW
    , pwm_4     :: PWMW
    , pwm_5     :: PWMW
    , pwm_6     :: PWMW
    , pwm_7     :: PWMW
    , pwm_8     :: PWMW
    , pwm_9     :: PWMW
    , pwm_10    :: PWMW
    , pwm_11    :: PWMW

    , npx_pwm_0 :: NeoPixelPWMW

    , exti_pa_0 :: EXTIW
    , exti_pa_5 :: EXTIW


    , etc       :: PageAddr
    }


gd32f3x0 :: String -> String -> MCUmod GD32F3x0
gd32f3x0 = MCUmod $ mkMCU G.systemClock makeMac inclGD32F3x0 GD32F3x0
    { uart_0    = mkUART usart0
                         rcu_usart0
                         usart0_irqn
                         dma_ch1
                         dma_channel1_2_irqn
                         (pb_6 af_0)
                         (pb_7 af_0)

    , uart_1    = mkUART usart1
                         rcu_usart1
                         usart1_irqn
                         dma_ch3
                         dma_channel3_4_irqn
                         (pa_3 af_1)
                         (pa_2 af_1)

    , in_pa_0   = mkInput pa_0
    , in_pa_1   = mkInput pa_1
    , in_pa_2   = mkInput pa_2
    , in_pa_3   = mkInput pa_3
    , in_pa_4   = mkInput pa_4
    , in_pa_5   = mkInput pa_5
    , in_pa_6   = mkInput pa_6
    , in_pa_7   = mkInput pa_7
    , in_pa_8   = mkInput pa_8
    , in_pa_9   = mkInput pa_9
    , in_pa_10  = mkInput pa_10
    , in_pa_11  = mkInput pa_11
    , in_pa_12  = mkInput pa_12
    , in_pa_13  = mkInput pa_13
    , in_pa_14  = mkInput pa_14
    , in_pa_15  = mkInput pa_15

    , in_pb_0   = mkInput pb_0
    , in_pb_1   = mkInput pb_1
    , in_pb_2   = mkInput pb_2
    , in_pb_3   = mkInput pb_3
    , in_pb_4   = mkInput pb_4
    , in_pb_5   = mkInput pb_5
    , in_pb_6   = mkInput pb_6
    , in_pb_7   = mkInput pb_7
    , in_pb_8   = mkInput pb_8
    , in_pb_9   = mkInput pb_9
    , in_pb_10  = mkInput pb_10
    , in_pb_11  = mkInput pb_11
    , in_pb_12  = mkInput pb_12
    , in_pb_13  = mkInput pb_13
    , in_pb_14  = mkInput pb_14
    , in_pb_15  = mkInput pb_15

    , out_pa_0  = mkOutput pa_0
    , out_pa_1  = mkOutput pa_1
    , out_pa_2  = mkOutput pa_2
    , out_pa_3  = mkOutput pa_3
    , out_pa_4  = mkOutput pa_4
    , out_pa_5  = mkOutput pa_5
    , out_pa_6  = mkOutput pa_6
    , out_pa_7  = mkOutput pa_7
    , out_pa_8  = mkOutput pa_8
    , out_pa_9  = mkOutput pa_9
    , out_pa_10 = mkOutput pa_10
    , out_pa_11 = mkOutput pa_11
    , out_pa_12 = mkOutput pa_12
    , out_pa_13 = mkOutput pa_13
    , out_pa_14 = mkOutput pa_14
    , out_pa_15 = mkOutput pa_15

    , out_pb_0  = mkOutput pb_0
    , out_pb_1  = mkOutput pb_1
    , out_pb_2  = mkOutput pb_2
    , out_pb_3  = mkOutput pb_3
    , out_pb_4  = mkOutput pb_4
    , out_pb_5  = mkOutput pb_5
    , out_pb_6  = mkOutput pb_6
    , out_pb_7  = mkOutput pb_7
    , out_pb_8  = mkOutput pb_8
    , out_pb_9  = mkOutput pb_9
    , out_pb_10 = mkOutput pb_10
    , out_pb_11 = mkOutput pb_11
    , out_pb_12 = mkOutput pb_12
    , out_pb_13 = mkOutput pb_13
    , out_pb_14 = mkOutput pb_14
    , out_pb_15 = mkOutput pb_15

    , od_pa_8   = mkOpenDrain pa_8

    , tim_0   =  cfg_timer_0
    , tim_1   =  cfg_timer_1
    , tim_2   =  cfg_timer_2
    , tim_14  =  cfg_timer_14
    , tim_15  =  cfg_timer_15


    , pwm_0     = mkPWM cfg_timer_1
                        timer_ch_0
                        (pa_0 af_2)

    , pwm_1     = mkPWM cfg_timer_1
                        timer_ch_1
                        (pa_1 af_2)

    , pwm_2     = mkPWM cfg_timer_1
                        timer_ch_2
                        (pa_2 af_2)

    , pwm_3     = mkPWM cfg_timer_1
                        timer_ch_3
                        (pa_3 af_2)

    , pwm_4     = mkPWM cfg_timer_2
                        timer_ch_0
                        (pa_6 af_1)

    , pwm_5     = mkPWM cfg_timer_2
                        timer_ch_1
                        (pa_7 af_1)

    , pwm_6     = mkPWM cfg_timer_2
                        timer_ch_2
                        (pb_0 af_1)

    , pwm_7     = mkPWM cfg_timer_2
                        timer_ch_3
                        (pb_1 af_1)

    , pwm_8     = mkPWM cfg_timer_0
                        timer_ch_0
                        (pa_8 af_2)

    , pwm_9     = mkPWM cfg_timer_0
                        timer_ch_1
                        (pa_9 af_2)

    , pwm_10    = mkPWM cfg_timer_0
                        timer_ch_2
                        (pa_10 af_2)

    , pwm_11    = mkPWM cfg_timer_0
                        timer_ch_3
                        (pa_11 af_2)

    , npx_pwm_0 = mkNeoPixelPWM cfg_timer_15
                                timer_ch_0 dma_ch2
                                (pb_8 af_2)

    , exti_pa_0 = mkEXTI (mkInput pa_0)
                         exti0_1_irqn
                         exti_source_gpioa
                         exti_source_pin0
                         exti_0

    , exti_pa_5 = mkEXTI (mkInput pa_5)
                         exti4_15_irqn
                         exti_source_gpioa
                         exti_source_pin5
                         exti_5



    , etc = mkPage 0x800_fc00
    }



gd32f330k8u6 :: MCUmod GD32F3x0
gd32f330k8u6 = gd32f3x0 "gd32f330" "k8u6"
