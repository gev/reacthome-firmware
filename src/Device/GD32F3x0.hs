{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE RankNTypes            #-}


module Device.GD32F3x0 where

import           Control.Monad.State
import           Core.Context
import           Data.Display.FrameBuffer.NeoPixel
import           Device.GD32F3x0.ADC
import           Device.GD32F3x0.DAC
import           Device.GD32F3x0.Display.NeoPixel
import           Device.GD32F3x0.EXTI
import           Device.GD32F3x0.Flash
import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.GPIO.Input
import           Device.GD32F3x0.GPIO.Mode
import           Device.GD32F3x0.GPIO.OpenDrain
import           Device.GD32F3x0.GPIO.Output
import           Device.GD32F3x0.I2C
import           Device.GD32F3x0.Mac               (makeMac)
import           Device.GD32F3x0.PWM
import           Device.GD32F3x0.SystemClock       as G
import           Device.GD32F3x0.SysTick
import           Device.GD32F3x0.Timer             (Timer, cfg_timer_0,
                                                    cfg_timer_1, cfg_timer_14,
                                                    cfg_timer_15, cfg_timer_2)
import           Device.GD32F3x0.Touch
import           Device.GD32F3x0.UART
import           GHC.TypeNats
import           Interface.GPIO.Port
import           Interface.Mac                     (Mac)
import           Interface.MCU
import           Interface.OneWire
import           Interface.SystemClock             (SystemClock, systemClock)
import qualified Interface.Timer                   as I
import           Ivory.Language
import           Support.Device.GD32F3x0
import           Support.Device.GD32F3x0.ADC
import           Support.Device.GD32F3x0.DMA
import           Support.Device.GD32F3x0.EXTI
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.I2C
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.RCU       as R
import           Support.Device.GD32F3x0.SYSCFG
import           Support.Device.GD32F3x0.Timer
import           Support.Device.GD32F3x0.USART



type UART'      = forall m rn tn. MonadState Context m => KnownNat rn => KnownNat tn => m (UART rn tn)
type I2C'       = forall m n.     MonadState Context m => KnownNat n => m (I2C n)
type Input'     = forall m.       MonadState Context m => GPIO_PUPD -> m Input
type Output'    = forall m.       MonadState Context m => GPIO_PUPD -> m Output
type OpenDrain' = forall m.       MonadState Context m => m OpenDrain
type ADC'       = forall m.       MonadState Context m => m ADC
type DAC'       = forall m.       MonadState Context m => m DAC
type Timer'     = forall m.       MonadState Context m => Uint32 -> Uint32 -> m Timer
type PWM'       = forall m.       MonadState Context m => Uint32 -> Uint32 -> m PWM
type NeoPixel'  = forall m.       MonadState Context m => m NeoPixel
type EXTI'      = forall m.       MonadState Context m => m EXTI
type OneWire'   = forall m.       MonadState Context m => m OpenDrain -> m OneWire
type Touch'     = forall m.       MonadState Context m => Uint32 -> IFloat -> IFloat -> m Touch


data GD32F3x0 = GD32F3x0
    { uart_0     :: UART'
    , uart_1     :: UART'

    , in_pa_0    :: Input'
    , in_pa_1    :: Input'
    , in_pa_2    :: Input'
    , in_pa_3    :: Input'
    , in_pa_4    :: Input'
    , in_pa_5    :: Input'
    , in_pa_6    :: Input'
    , in_pa_7    :: Input'
    , in_pa_8    :: Input'
    , in_pa_9    :: Input'
    , in_pa_10   :: Input'
    , in_pa_11   :: Input'
    , in_pa_12   :: Input'
    , in_pa_13   :: Input'
    , in_pa_14   :: Input'
    , in_pa_15   :: Input'

    , in_pb_0    :: Input'
    , in_pb_1    :: Input'
    , in_pb_2    :: Input'
    , in_pb_3    :: Input'
    , in_pb_4    :: Input'
    , in_pb_5    :: Input'
    , in_pb_6    :: Input'
    , in_pb_7    :: Input'
    , in_pb_8    :: Input'
    , in_pb_9    :: Input'
    , in_pb_10   :: Input'
    , in_pb_11   :: Input'
    , in_pb_12   :: Input'
    , in_pb_13   :: Input'
    , in_pb_14   :: Input'
    , in_pb_15   :: Input'

    , out_pa_0   :: Output'
    , out_pa_1   :: Output'
    , out_pa_2   :: Output'
    , out_pa_3   :: Output'
    , out_pa_4   :: Output'
    , out_pa_5   :: Output'
    , out_pa_6   :: Output'
    , out_pa_7   :: Output'
    , out_pa_8   :: Output'
    , out_pa_9   :: Output'
    , out_pa_10  :: Output'
    , out_pa_11  :: Output'
    , out_pa_12  :: Output'
    , out_pa_13  :: Output'
    , out_pa_14  :: Output'
    , out_pa_15  :: Output'

    , out_pb_0   :: Output'
    , out_pb_1   :: Output'
    , out_pb_2   :: Output'
    , out_pb_3   :: Output'
    , out_pb_4   :: Output'
    , out_pb_5   :: Output'
    , out_pb_6   :: Output'
    , out_pb_7   :: Output'
    , out_pb_8   :: Output'
    , out_pb_9   :: Output'
    , out_pb_10  :: Output'
    , out_pb_11  :: Output'
    , out_pb_12  :: Output'
    , out_pb_13  :: Output'
    , out_pb_14  :: Output'
    , out_pb_15  :: Output'

    , od_pa_5    :: OpenDrain'
    , od_pa_8    :: OpenDrain'
    , od_pa_15   :: OpenDrain'

    , timer_0    :: Timer'
    , timer_1    :: Timer'
    , timer_2    :: Timer'
    , timer_14   :: Timer'
    , timer_15   :: Timer'

    , pwm_0      :: PWM'
    , pwm_1      :: PWM'
    , pwm_2      :: PWM'
    , pwm_3      :: PWM'
    , pwm_4      :: PWM'
    , pwm_5      :: PWM'
    , pwm_6      :: PWM'
    , pwm_7      :: PWM'
    , pwm_8      :: PWM'
    , pwm_9      :: PWM'
    , pwm_10     :: PWM'
    , pwm_11     :: PWM'

    , npx_pwm_0  :: NeoPixel'
    , npx_pwm_1  :: NeoPixel'
    , npx_pwm_3  :: NeoPixel'

    , exti_pa_0  :: EXTI'
    , exti_pa_5  :: EXTI'
    , exti_pb_7  :: EXTI'

    , ow_0       :: OneWire'
    , ow_1       :: OneWire'

    , etc        :: PageAddr

    , i2c_0      :: I2C'

    , adc_pa_0   :: ADC'
    , adc_pa_1   :: ADC'
    , adc_pa_5   :: ADC'
    , adc_pa_6   :: ADC'
    , adc_pa_7   :: ADC'

    , dac_pa_4   :: DAC'

    , touch_pa4  :: Touch'
    , touch_pa5  :: Touch'
    , touch_pa7  :: Touch'
    , touch_pa15 :: Touch'
    , touch_pb0  :: Touch'
    , touch_pb1  :: Touch'
    , touch_pb2  :: Touch'
    , touch_pb3  :: Touch'
    , touch_pb4  :: Touch'
    , touch_pb5  :: Touch'
    , touch_pb6  :: Touch'
    , touch_pb7  :: Touch'
    , touch_pb8  :: Touch'

    }


gd32f3x0 :: String -> String -> MCU GD32F3x0
gd32f3x0 = MCU $ mkPlatform G.systemClock makeMac inclGD32F3x0 GD32F3x0
    { uart_0    = mkUART usart0
                         rcu_usart0
                         usart0_irqn
                         (pb_6 af_0)
                         (pb_7 af_0)

    , uart_1    = mkUART usart1
                         rcu_usart1
                         usart1_irqn
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

    , od_pa_5   = mkOpenDrain pa_5
    , od_pa_8   = mkOpenDrain pa_8
    , od_pa_15  = mkOpenDrain pa_15

    , timer_0   =  cfg_timer_0
    , timer_1   =  cfg_timer_1
    , timer_2   =  cfg_timer_2
    , timer_14  =  cfg_timer_14
    , timer_15  =  cfg_timer_15


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
                                dma_channel1_2_irqn ch0cv
                                (pb_8 af_2)

    , npx_pwm_1 = mkNeoPixelPWM cfg_timer_15
                                timer_ch_0 dma_ch2
                                dma_channel1_2_irqn ch0cv
                                (pa_6 af_5)

    , npx_pwm_3 = mkNeoPixelPWM cfg_timer_2
                                timer_ch_1 dma_ch2
                                dma_channel1_2_irqn ch1cv
                                (pa_7 af_1)

    , exti_pa_0 = mkEXTI pa_0
                         exti0_1_irqn
                         exti_source_gpioa
                         exti_source_pin0
                         exti_0

    , exti_pa_5 = mkEXTI pa_5
                         exti4_15_irqn
                         exti_source_gpioa
                         exti_source_pin5
                         exti_5

    , exti_pb_7 = mkEXTI pb_7
                         exti4_15_irqn
                         exti_source_gpiob
                         exti_source_pin7
                         exti_7


    , ow_0  = mkOneWire cfg_timer_15
    , ow_1  = mkOneWire cfg_timer_2

    , etc = mkPage 0x800_bc00

    , i2c_0 = mkI2C i2c0
                    rcu_i2c0
                    i2c0_ev_irqn
                    i2c0_er_irqn
                    (pa_10 af_4)
                    (pa_9  af_4)

    , adc_pa_0 = mkADC (pa_0 analog) 0
    , adc_pa_1 = mkADC (pa_1 analog) 1
    , adc_pa_5 = mkADC (pa_5 analog) 5
    , adc_pa_6 = mkADC (pa_6 analog) 6
    , adc_pa_7 = mkADC (pa_7 analog) 7

    , dac_pa_4 = mkDAC (pa_4 analog)


    , touch_pa4  = mkTouch gpioa gpio_pin_4  rcu_gpioa
    , touch_pa5  = mkTouch gpioa gpio_pin_5  rcu_gpioa
    , touch_pa7  = mkTouch gpioa gpio_pin_7  rcu_gpioa
    , touch_pa15 = mkTouch gpioa gpio_pin_15 rcu_gpioa
    , touch_pb0  = mkTouch gpiob gpio_pin_0  rcu_gpiob
    , touch_pb1  = mkTouch gpiob gpio_pin_1  rcu_gpiob
    , touch_pb2  = mkTouch gpiob gpio_pin_2  rcu_gpiob
    , touch_pb3  = mkTouch gpiob gpio_pin_3  rcu_gpiob
    , touch_pb4  = mkTouch gpiob gpio_pin_4  rcu_gpiob
    , touch_pb5  = mkTouch gpiob gpio_pin_5  rcu_gpiob
    , touch_pb6  = mkTouch gpiob gpio_pin_6  rcu_gpiob
    , touch_pb7  = mkTouch gpiob gpio_pin_7  rcu_gpiob
    , touch_pb8  = mkTouch gpiob gpio_pin_8  rcu_gpiob

    }



gd32f330k8u6 :: MCU GD32F3x0
gd32f330k8u6 = gd32f3x0 "gd32f330" "k8u6"

gd32f350k8u6 :: MCU GD32F3x0
gd32f350k8u6 = gd32f3x0 "gd32f350" "k8u6"


instance Pull GD32F3x0 GPIO_PUPD where
    pullNone _ = gpio_pupd_none
    pullUp   _ = gpio_pupd_pullup
    pullDown _ = gpio_pupd_pulldown
