{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}


module Device.GD32F3x0 where

import           Control.Monad.Writer
import           Core.Context
import           Device.GD32F3x0.EXTI
import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.GPIO.Input
import           Device.GD32F3x0.GPIO.Output
import           Device.GD32F3x0.Mac            (makeMac)
import           Device.GD32F3x0.NeoPixel
import           Device.GD32F3x0.PWM
import           Device.GD32F3x0.SystemClock    as G
import           Device.GD32F3x0.SysTick
import           Device.GD32F3x0.Timer
import           Device.GD32F3x0.UART
import           Interface.Mac                  (Mac)
import           Interface.MCU
import           Interface.SystemClock          (SystemClock, systemClock)
import           Ivory.Language
import           Support.Device.GD32F3x0
import           Support.Device.GD32F3x0.DMA
import           Support.Device.GD32F3x0.EXTI
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.RCU    as R
import           Support.Device.GD32F3x0.SYSCFG
import           Support.Device.GD32F3x0.Timer
import           Support.Device.GD32F3x0.USART



type UARTW        = forall m. MonadWriter Context m => m UART
type InputW       = forall m. MonadWriter Context m => m Input
type OutputW      = forall m. MonadWriter Context m => m Output
type PWMW         = forall m. MonadWriter Context m => Uint32 -> Uint32 -> m PWM
type NeoPixelPWMW = forall m. MonadWriter Context m => m NeoPixelPWM
type EXTIW        = forall m. MonadWriter Context m => m EXTI


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
    }


gd32f3x0 :: String -> String -> MCUmod GD32F3x0
gd32f3x0 = MCUmod $ mkMCU G.systemClock makeMac inclGD32F3x0 GD32F3x0
    { uart_0    = mkUART usart0
                         rcu_usart0
                         usart0_irqn
                         dma_ch1
                         dma_channel1_2_irqn
                         (pb_6 $ AF gpio_af_0)
                         (pb_7 $ AF gpio_af_0)

    , uart_1    = mkUART usart1
                         rcu_usart1
                         usart1_irqn
                         dma_ch3
                         dma_channel3_4_irqn
                         (pa_3 $ AF gpio_af_1)
                         (pa_2 $ AF gpio_af_1)

    , in_pa_0   = input pa_0
    , in_pa_1   = input pa_1
    , in_pa_2   = input pa_2
    , in_pa_3   = input pa_3
    , in_pa_4   = input pa_4
    , in_pa_5   = input pa_5
    , in_pa_6   = input pa_6
    , in_pa_7   = input pa_7
    , in_pa_8   = input pa_8
    , in_pa_9   = input pa_9
    , in_pa_10  = input pa_10
    , in_pa_11  = input pa_11
    , in_pa_12  = input pa_12
    , in_pa_13  = input pa_13
    , in_pa_14  = input pa_14
    , in_pa_15  = input pa_15

    , in_pb_0   = input pb_0
    , in_pb_1   = input pb_1
    , in_pb_2   = input pb_2
    , in_pb_3   = input pb_3
    , in_pb_4   = input pb_4
    , in_pb_5   = input pb_5
    , in_pb_6   = input pb_6
    , in_pb_7   = input pb_7
    , in_pb_8   = input pb_8
    , in_pb_9   = input pb_9
    , in_pb_10  = input pb_10
    , in_pb_11  = input pb_11
    , in_pb_12  = input pb_12
    , in_pb_13  = input pb_13
    , in_pb_14  = input pb_14
    , in_pb_15  = input pb_15

    , out_pa_0  = output pa_0
    , out_pa_1  = output pa_1
    , out_pa_2  = output pa_2
    , out_pa_3  = output pa_3
    , out_pa_4  = output pa_4
    , out_pa_5  = output pa_5
    , out_pa_6  = output pa_6
    , out_pa_7  = output pa_7
    , out_pa_8  = output pa_8
    , out_pa_9  = output pa_9
    , out_pa_10 = output pa_10
    , out_pa_11 = output pa_11
    , out_pa_12 = output pa_12
    , out_pa_13 = output pa_13
    , out_pa_14 = output pa_14
    , out_pa_15 = output pa_15

    , out_pb_0  = output pb_0
    , out_pb_1  = output pb_1
    , out_pb_2  = output pb_2
    , out_pb_3  = output pb_3
    , out_pb_4  = output pb_4
    , out_pb_5  = output pb_5
    , out_pb_6  = output pb_6
    , out_pb_7  = output pb_7
    , out_pb_8  = output pb_8
    , out_pb_9  = output pb_9
    , out_pb_10 = output pb_10
    , out_pb_11 = output pb_11
    , out_pb_12 = output pb_12
    , out_pb_13 = output pb_13
    , out_pb_14 = output pb_14
    , out_pb_15 = output pb_15

    , pwm_0     = mkPWM pwm_timer_1
                        timer_ch_0
                        (pa_0  $ AF gpio_af_2)

    , pwm_1     = mkPWM pwm_timer_1
                        timer_ch_1
                        (pa_1  $ AF gpio_af_2)

    , pwm_2     = mkPWM pwm_timer_1
                        timer_ch_2
                        (pa_2  $ AF gpio_af_2)

    , pwm_3     = mkPWM pwm_timer_1
                        timer_ch_3
                        (pa_3  $ AF gpio_af_2)

    , pwm_4     = mkPWM pwm_timer_2
                        timer_ch_0
                        (pa_6  $ AF gpio_af_1)

    , pwm_5     = mkPWM pwm_timer_2
                        timer_ch_1
                        (pa_7  $ AF gpio_af_1)

    , pwm_6     = mkPWM pwm_timer_2
                        timer_ch_2
                        (pb_0  $ AF gpio_af_1)

    , pwm_7     = mkPWM pwm_timer_2
                        timer_ch_3
                        (pb_1  $ AF gpio_af_1)

    , pwm_8     = mkPWM pwm_timer_0
                        timer_ch_0
                        (pa_8  $ AF gpio_af_2)

    , pwm_9     = mkPWM pwm_timer_0
                        timer_ch_1
                        (pa_9  $ AF gpio_af_2)

    , pwm_10    = mkPWM pwm_timer_0
                        timer_ch_2
                        (pa_10 $ AF gpio_af_2)

    , pwm_11    = mkPWM pwm_timer_0
                        timer_ch_3
                        (pa_11 $ AF gpio_af_2)

    , npx_pwm_0 = mkNeoPixelPWM pwm_timer_15
                                timer_ch_0 dma_ch2
                                dma_channel1_2_irqn
                                (pb_8 $ AF gpio_af_2)

    , exti_pa_0 = mkEXTI (input pa_0)
                         exti0_1_irqn
                         exti_source_gpioa
                         exti_source_pin0
                         exti_0

    , exti_pa_5 = mkEXTI (input pa_5)
                         exti4_15_irqn
                         exti_source_gpioa
                         exti_source_pin5
                         exti_5

    }



gd32f330k8u6 :: MCUmod GD32F3x0
gd32f330k8u6 = gd32f3x0 "gd32f330" "k8u6"
