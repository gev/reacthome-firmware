{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}

module Device.GD32F4xx where

import           Control.Monad.State
import           Core.Context
import           Device.GD32F4xx.Display.NeoPixel
import           Device.GD32F4xx.Flash
import           Device.GD32F4xx.GPIO
import           Device.GD32F4xx.GPIO.Input
import           Device.GD32F4xx.GPIO.Output
import           Device.GD32F4xx.Mac              (makeMac)
import           Device.GD32F4xx.PWM
import           Device.GD32F4xx.SystemClock      as G
import           Device.GD32F4xx.SysTick
import           Device.GD32F4xx.Timer
import           Device.GD32F4xx.UART
import           Interface.Mac                    (Mac)
import           Interface.MCU
import           Interface.SystemClock            (SystemClock)
import           Ivory.Language
import           Support.Device.GD32F4xx
import           Support.Device.GD32F4xx.DMA
import           Support.Device.GD32F4xx.GPIO
import           Support.Device.GD32F4xx.IRQ
import           Support.Device.GD32F4xx.RCU
import           Support.Device.GD32F4xx.Timer
import           Support.Device.GD32F4xx.USART




type UARTW         = forall m. MonadState Context m => m UART
type InputW        = forall m. MonadState Context m => m Input
type OutputW       = forall m. MonadState Context m => m Output
type PWMW          = forall m. MonadState Context m => Uint32 -> Uint32 -> m PWM
type NeoPixelPWMW  = forall m. MonadState Context m => m NeoPixelPWM



etcPage = mkPage 0x800_fc00


data GD32F4xx = GD32F4xx
    { uart_0    :: UARTW
    , uart_1    :: UARTW
    , uart_2    :: UARTW
    , uart_3    :: UARTW
    , uart_5    :: UARTW
    , uart_7    :: UARTW

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

    , in_pc_0   :: InputW
    , in_pc_1   :: InputW
    , in_pc_2   :: InputW
    , in_pc_3   :: InputW
    , in_pc_4   :: InputW
    , in_pc_5   :: InputW
    , in_pc_6   :: InputW
    , in_pc_7   :: InputW
    , in_pc_8   :: InputW
    , in_pc_9   :: InputW
    , in_pc_10  :: InputW
    , in_pc_11  :: InputW
    , in_pc_12  :: InputW
    , in_pc_13  :: InputW
    , in_pc_14  :: InputW
    , in_pc_15  :: InputW

    , in_pd_0   :: InputW
    , in_pd_1   :: InputW
    , in_pd_2   :: InputW
    , in_pd_3   :: InputW
    , in_pd_4   :: InputW
    , in_pd_5   :: InputW
    , in_pd_6   :: InputW
    , in_pd_7   :: InputW
    , in_pd_8   :: InputW
    , in_pd_9   :: InputW
    , in_pd_10  :: InputW
    , in_pd_11  :: InputW
    , in_pd_12  :: InputW
    , in_pd_13  :: InputW
    , in_pd_14  :: InputW
    , in_pd_15  :: InputW

    , in_pe_0   :: InputW
    , in_pe_1   :: InputW
    , in_pe_2   :: InputW
    , in_pe_3   :: InputW
    , in_pe_4   :: InputW
    , in_pe_5   :: InputW
    , in_pe_6   :: InputW
    , in_pe_7   :: InputW
    , in_pe_8   :: InputW
    , in_pe_9   :: InputW
    , in_pe_10  :: InputW
    , in_pe_11  :: InputW
    , in_pe_12  :: InputW
    , in_pe_13  :: InputW
    , in_pe_14  :: InputW
    , in_pe_15  :: InputW


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

    , out_pc_0  :: OutputW
    , out_pc_1  :: OutputW
    , out_pc_2  :: OutputW
    , out_pc_3  :: OutputW
    , out_pc_4  :: OutputW
    , out_pc_5  :: OutputW
    , out_pc_6  :: OutputW
    , out_pc_7  :: OutputW
    , out_pc_8  :: OutputW
    , out_pc_9  :: OutputW
    , out_pc_10 :: OutputW
    , out_pc_11 :: OutputW
    , out_pc_12 :: OutputW
    , out_pc_13 :: OutputW
    , out_pc_14 :: OutputW
    , out_pc_15 :: OutputW

    , out_pd_0  :: OutputW
    , out_pd_1  :: OutputW
    , out_pd_2  :: OutputW
    , out_pd_3  :: OutputW
    , out_pd_4  :: OutputW
    , out_pd_5  :: OutputW
    , out_pd_6  :: OutputW
    , out_pd_7  :: OutputW
    , out_pd_8  :: OutputW
    , out_pd_9  :: OutputW
    , out_pd_10 :: OutputW
    , out_pd_11 :: OutputW
    , out_pd_12 :: OutputW
    , out_pd_13 :: OutputW
    , out_pd_14 :: OutputW
    , out_pd_15 :: OutputW

    , out_pe_0  :: OutputW
    , out_pe_1  :: OutputW
    , out_pe_2  :: OutputW
    , out_pe_3  :: OutputW
    , out_pe_4  :: OutputW
    , out_pe_5  :: OutputW
    , out_pe_6  :: OutputW
    , out_pe_7  :: OutputW
    , out_pe_8  :: OutputW
    , out_pe_9  :: OutputW
    , out_pe_10 :: OutputW
    , out_pe_11 :: OutputW
    , out_pe_12 :: OutputW
    , out_pe_13 :: OutputW
    , out_pe_14 :: OutputW
    , out_pe_15 :: OutputW

    , pwm_0     :: PWMW
    , pwm_1     :: PWMW
    , pwm_2     :: PWMW
    , pwm_3     :: PWMW

    , npx_pwm_0 :: NeoPixelPWMW
    , npx_pwm_1 :: NeoPixelPWMW
    , npx_pwm_2 :: NeoPixelPWMW
    , npx_pwm_3 :: NeoPixelPWMW
    }


gd32f4xx :: String -> String -> MCUmod GD32F4xx
gd32f4xx = MCUmod $ mkMCU G.systemClock makeMac inclGD32F4xx GD32F4xx
    { uart_0    = mkUART usart0
                         rcu_usart0
                         usart0_irqn
                         rcu_dma1
                         dma1
                         dma_ch7
                         dma_subperi4
                         dma1_channel7_irqn
                         (pb_7 $ AF gpio_af_7)
                         (pb_6 $ AF gpio_af_7)

    , uart_1    = mkUART usart1
                         rcu_usart1
                         usart1_irqn
                         rcu_dma0
                         dma0
                         dma_ch6
                         dma_subperi4
                         dma0_channel6_irqn
                         (pd_6 $ AF gpio_af_7)
                         (pd_5 $ AF gpio_af_7)

    , uart_2    = mkUART usart2
                         rcu_usart2
                         usart2_irqn
                         rcu_dma0
                         dma0
                         dma_ch3
                         dma_subperi4
                         dma0_channel3_irqn
                         (pd_9 $ AF gpio_af_7)
                         (pd_8 $ AF gpio_af_7)

    , uart_3    = mkUART uart3
                         rcu_uart3
                         uart3_irqn
                         rcu_dma0
                         dma0
                         dma_ch4
                         dma_subperi4
                         dma0_channel4_irqn
                         (pc_11 $ AF gpio_af_8)
                         (pc_10 $ AF gpio_af_8)

    , uart_5    = mkUART usart5
                         rcu_usart5
                         usart5_irqn
                         rcu_dma1
                         dma1
                         dma_ch6
                         dma_subperi5
                         dma1_channel6_irqn
                         (pc_7 $ AF gpio_af_8)
                         (pc_6 $ AF gpio_af_8)

    , uart_7    = mkUART uart7
                         rcu_uart7
                         uart7_irqn
                         rcu_dma0
                         dma0
                         dma_ch0
                         dma_subperi5
                         dma0_channel0_irqn
                         (pe_0 $ AF gpio_af_8)
                         (pe_1 $ AF gpio_af_8)


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

    , in_pc_0   = input pc_0
    , in_pc_1   = input pc_1
    , in_pc_2   = input pc_2
    , in_pc_3   = input pc_3
    , in_pc_4   = input pc_4
    , in_pc_5   = input pc_5
    , in_pc_6   = input pc_6
    , in_pc_7   = input pc_7
    , in_pc_8   = input pc_8
    , in_pc_9   = input pc_9
    , in_pc_10  = input pc_10
    , in_pc_11  = input pc_11
    , in_pc_12  = input pc_12
    , in_pc_13  = input pc_13
    , in_pc_14  = input pc_14
    , in_pc_15  = input pc_15

    , in_pd_0   = input pd_0
    , in_pd_1   = input pd_1
    , in_pd_2   = input pd_2
    , in_pd_3   = input pd_3
    , in_pd_4   = input pd_4
    , in_pd_5   = input pd_5
    , in_pd_6   = input pd_6
    , in_pd_7   = input pd_7
    , in_pd_8   = input pd_8
    , in_pd_9   = input pd_9
    , in_pd_10  = input pd_10
    , in_pd_11  = input pd_11
    , in_pd_12  = input pd_12
    , in_pd_13  = input pd_13
    , in_pd_14  = input pd_14
    , in_pd_15  = input pd_15

    , in_pe_0   = input pe_0
    , in_pe_1   = input pe_1
    , in_pe_2   = input pe_2
    , in_pe_3   = input pe_3
    , in_pe_4   = input pe_4
    , in_pe_5   = input pe_5
    , in_pe_6   = input pe_6
    , in_pe_7   = input pe_7
    , in_pe_8   = input pe_8
    , in_pe_9   = input pe_9
    , in_pe_10  = input pe_10
    , in_pe_11  = input pe_11
    , in_pe_12  = input pe_12
    , in_pe_13  = input pe_13
    , in_pe_14  = input pe_14
    , in_pe_15  = input pe_15

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

    , out_pc_0  = output pc_0
    , out_pc_1  = output pc_1
    , out_pc_2  = output pc_2
    , out_pc_3  = output pc_3
    , out_pc_4  = output pc_4
    , out_pc_5  = output pc_5
    , out_pc_6  = output pc_6
    , out_pc_7  = output pc_7
    , out_pc_8  = output pc_8
    , out_pc_9  = output pc_9
    , out_pc_10 = output pc_10
    , out_pc_11 = output pc_11
    , out_pc_12 = output pc_12
    , out_pc_13 = output pc_13
    , out_pc_14 = output pc_14
    , out_pc_15 = output pc_15

    , out_pd_0  = output pd_0
    , out_pd_1  = output pd_1
    , out_pd_2  = output pd_2
    , out_pd_3  = output pd_3
    , out_pd_4  = output pd_4
    , out_pd_5  = output pd_5
    , out_pd_6  = output pd_6
    , out_pd_7  = output pd_7
    , out_pd_8  = output pd_8
    , out_pd_9  = output pd_9
    , out_pd_10 = output pd_10
    , out_pd_11 = output pd_11
    , out_pd_12 = output pd_12
    , out_pd_13 = output pd_13
    , out_pd_14 = output pd_14
    , out_pd_15 = output pd_15

    , out_pe_0  = output pe_0
    , out_pe_1  = output pe_1
    , out_pe_2  = output pe_2
    , out_pe_3  = output pe_3
    , out_pe_4  = output pe_4
    , out_pe_5  = output pe_5
    , out_pe_6  = output pe_6
    , out_pe_7  = output pe_7
    , out_pe_8  = output pe_8
    , out_pe_9  = output pe_9
    , out_pe_10 = output pe_10
    , out_pe_11 = output pe_11
    , out_pe_12 = output pe_12
    , out_pe_13 = output pe_13
    , out_pe_14 = output pe_14
    , out_pe_15 = output pe_15


    , pwm_0     = mkPWM pwm_timer_3
                        timer_ch_0
                        (pd_12  $ AF gpio_af_2)
    , pwm_1     = mkPWM pwm_timer_3
                        timer_ch_1
                        (pd_13  $ AF gpio_af_2)
    , pwm_2     = mkPWM pwm_timer_3
                        timer_ch_2
                        (pd_14  $ AF gpio_af_2)
    , pwm_3     = mkPWM pwm_timer_3
                        timer_ch_3
                        (pd_15  $ AF gpio_af_2)


    , npx_pwm_0 = mkNeoPixelPWM pwm_timer_2
                                timer_ch_0 rcu_dma0
                                dma0 dma_ch2
                                dma_subperi5 ch0cv
                                (pb_4 $ AF gpio_af_2)

    , npx_pwm_1 = mkNeoPixelPWM pwm_timer_2
                                timer_ch_1 rcu_dma0
                                dma0 dma_ch2
                                dma_subperi5 ch1cv
                                (pb_5 $ AF gpio_af_2)

    , npx_pwm_2 = mkNeoPixelPWM pwm_timer_2
                                timer_ch_2 rcu_dma0
                                dma0 dma_ch2
                                dma_subperi5 ch2cv
                                (pb_0 $ AF gpio_af_2)

    , npx_pwm_3 = mkNeoPixelPWM pwm_timer_2
                                timer_ch_3 rcu_dma0
                                dma0 dma_ch2
                                dma_subperi5 ch3cv
                                (pc_7 $ AF gpio_af_2)
    }



gd32f450vgt6 :: MCUmod GD32F4xx
gd32f450vgt6 = gd32f4xx "gd32f450" "vgt6"

gd32f450vit6 :: MCUmod GD32F4xx
gd32f450vit6 = gd32f4xx "gd32f450" "vit6"
