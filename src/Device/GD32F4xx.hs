{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE RankNTypes            #-}

module Device.GD32F4xx where

import           Control.Monad.State
import           Core.Context
import           Data.Display.FrameBuffer.NeoPixel
import           Device.GD32F4xx.Display.NeoPixel
import           Device.GD32F4xx.ENET
import           Device.GD32F4xx.Flash
import           Device.GD32F4xx.GPIO
import           Device.GD32F4xx.GPIO.Input
import           Device.GD32F4xx.GPIO.Mode
import           Device.GD32F4xx.GPIO.OpenDrain
import           Device.GD32F4xx.GPIO.Output
import           Device.GD32F4xx.Mac               (makeMac)
import           Device.GD32F4xx.PWM
import           Device.GD32F4xx.SystemClock       as G
import           Device.GD32F4xx.SysTick
import           Device.GD32F4xx.Timer             (Timer, cfg_timer_1,
                                                    cfg_timer_2, cfg_timer_3,
                                                    cfg_timer_6, cfg_timer_7)
import           Device.GD32F4xx.UART
import           GHC.TypeNats
import           Interface.GPIO.Port
import           Interface.Mac                     (Mac)
import           Interface.MCU
import           Interface.OneWire
import           Interface.SystemClock             (SystemClock)
import           Ivory.Language
import           Support.Device.GD32F4xx
import           Support.Device.GD32F4xx.DMA
import           Support.Device.GD32F4xx.GPIO
import           Support.Device.GD32F4xx.IRQ
import           Support.Device.GD32F4xx.RCU
import           Support.Device.GD32F4xx.Timer
import           Support.Device.GD32F4xx.USART
import           Support.Device.GD32F4xx.FMC




type UART'      = forall m n. MonadState Context m => KnownNat n => m (UART n)
type Input'     = forall m.   MonadState Context m => GPIO_PUPD -> m Input
type Output'    = forall m.   MonadState Context m => GPIO_PUPD -> m Output
type OpenDrain' = forall m.   MonadState Context m => m OpenDrain
type Timer'     = forall m.   MonadState Context m => Uint32 -> Uint32 -> m Timer
type PWM'       = forall m.   MonadState Context m => Uint32 -> Uint32 -> m PWM
type NeoPixel'  = forall m.   MonadState Context m => m NeoPixel
type OneWire'   = forall m.   MonadState Context m => m OpenDrain -> m OneWire
type Enet'      = forall m.   MonadState Context m => m ENET
type PageAddr'  = forall m.   MonadState Context m => m PageAddr

data GD32F4xx = GD32F4xx
    { uart_0    :: UART'
    , uart_1    :: UART'
    , uart_2    :: UART'
    , uart_3    :: UART'
    , uart_5    :: UART'
    , uart_7    :: UART'

    , in_pa_0   :: Input'
    , in_pa_1   :: Input'
    , in_pa_2   :: Input'
    , in_pa_3   :: Input'
    , in_pa_4   :: Input'
    , in_pa_5   :: Input'
    , in_pa_6   :: Input'
    , in_pa_7   :: Input'
    , in_pa_8   :: Input'
    , in_pa_9   :: Input'
    , in_pa_10  :: Input'
    , in_pa_11  :: Input'
    , in_pa_12  :: Input'
    , in_pa_13  :: Input'
    , in_pa_14  :: Input'
    , in_pa_15  :: Input'

    , in_pb_0   :: Input'
    , in_pb_1   :: Input'
    , in_pb_2   :: Input'
    , in_pb_3   :: Input'
    , in_pb_4   :: Input'
    , in_pb_5   :: Input'
    , in_pb_6   :: Input'
    , in_pb_7   :: Input'
    , in_pb_8   :: Input'
    , in_pb_9   :: Input'
    , in_pb_10  :: Input'
    , in_pb_11  :: Input'
    , in_pb_12  :: Input'
    , in_pb_13  :: Input'
    , in_pb_14  :: Input'
    , in_pb_15  :: Input'

    , in_pc_0   :: Input'
    , in_pc_1   :: Input'
    , in_pc_2   :: Input'
    , in_pc_3   :: Input'
    , in_pc_4   :: Input'
    , in_pc_5   :: Input'
    , in_pc_6   :: Input'
    , in_pc_7   :: Input'
    , in_pc_8   :: Input'
    , in_pc_9   :: Input'
    , in_pc_10  :: Input'
    , in_pc_11  :: Input'
    , in_pc_12  :: Input'
    , in_pc_13  :: Input'
    , in_pc_14  :: Input'
    , in_pc_15  :: Input'

    , in_pd_0   :: Input'
    , in_pd_1   :: Input'
    , in_pd_2   :: Input'
    , in_pd_3   :: Input'
    , in_pd_4   :: Input'
    , in_pd_5   :: Input'
    , in_pd_6   :: Input'
    , in_pd_7   :: Input'
    , in_pd_8   :: Input'
    , in_pd_9   :: Input'
    , in_pd_10  :: Input'
    , in_pd_11  :: Input'
    , in_pd_12  :: Input'
    , in_pd_13  :: Input'
    , in_pd_14  :: Input'
    , in_pd_15  :: Input'

    , in_pe_0   :: Input'
    , in_pe_1   :: Input'
    , in_pe_2   :: Input'
    , in_pe_3   :: Input'
    , in_pe_4   :: Input'
    , in_pe_5   :: Input'
    , in_pe_6   :: Input'
    , in_pe_7   :: Input'
    , in_pe_8   :: Input'
    , in_pe_9   :: Input'
    , in_pe_10  :: Input'
    , in_pe_11  :: Input'
    , in_pe_12  :: Input'
    , in_pe_13  :: Input'
    , in_pe_14  :: Input'
    , in_pe_15  :: Input'


    , out_pa_0  :: Output'
    , out_pa_1  :: Output'
    , out_pa_2  :: Output'
    , out_pa_3  :: Output'
    , out_pa_4  :: Output'
    , out_pa_5  :: Output'
    , out_pa_6  :: Output'
    , out_pa_7  :: Output'
    , out_pa_8  :: Output'
    , out_pa_9  :: Output'
    , out_pa_10 :: Output'
    , out_pa_11 :: Output'
    , out_pa_12 :: Output'
    , out_pa_13 :: Output'
    , out_pa_14 :: Output'
    , out_pa_15 :: Output'

    , out_pb_0  :: Output'
    , out_pb_1  :: Output'
    , out_pb_2  :: Output'
    , out_pb_3  :: Output'
    , out_pb_4  :: Output'
    , out_pb_5  :: Output'
    , out_pb_6  :: Output'
    , out_pb_7  :: Output'
    , out_pb_8  :: Output'
    , out_pb_9  :: Output'
    , out_pb_10 :: Output'
    , out_pb_11 :: Output'
    , out_pb_12 :: Output'
    , out_pb_13 :: Output'
    , out_pb_14 :: Output'
    , out_pb_15 :: Output'

    , out_pc_0  :: Output'
    , out_pc_1  :: Output'
    , out_pc_2  :: Output'
    , out_pc_3  :: Output'
    , out_pc_4  :: Output'
    , out_pc_5  :: Output'
    , out_pc_6  :: Output'
    , out_pc_7  :: Output'
    , out_pc_8  :: Output'
    , out_pc_9  :: Output'
    , out_pc_10 :: Output'
    , out_pc_11 :: Output'
    , out_pc_12 :: Output'
    , out_pc_13 :: Output'
    , out_pc_14 :: Output'
    , out_pc_15 :: Output'

    , out_pd_0  :: Output'
    , out_pd_1  :: Output'
    , out_pd_2  :: Output'
    , out_pd_3  :: Output'
    , out_pd_4  :: Output'
    , out_pd_5  :: Output'
    , out_pd_6  :: Output'
    , out_pd_7  :: Output'
    , out_pd_8  :: Output'
    , out_pd_9  :: Output'
    , out_pd_10 :: Output'
    , out_pd_11 :: Output'
    , out_pd_12 :: Output'
    , out_pd_13 :: Output'
    , out_pd_14 :: Output'
    , out_pd_15 :: Output'

    , out_pe_0  :: Output'
    , out_pe_1  :: Output'
    , out_pe_2  :: Output'
    , out_pe_3  :: Output'
    , out_pe_4  :: Output'
    , out_pe_5  :: Output'
    , out_pe_6  :: Output'
    , out_pe_7  :: Output'
    , out_pe_8  :: Output'
    , out_pe_9  :: Output'
    , out_pe_10 :: Output'
    , out_pe_11 :: Output'
    , out_pe_12 :: Output'
    , out_pe_13 :: Output'
    , out_pe_14 :: Output'
    , out_pe_15 :: Output'

    , od_pb_3   :: OpenDrain'

    , timer_1   :: Timer'
    , timer_2   :: Timer'
    , timer_3   :: Timer'
    , timer_6   :: Timer'
    , timer_7   :: Timer'

    , pwm_0     :: PWM'
    , pwm_1     :: PWM'
    , pwm_2     :: PWM'
    , pwm_3     :: PWM'

    , npx_pwm_0 :: NeoPixel'
    , npx_pwm_1 :: NeoPixel'

    , ow_0      :: OneWire'

    , eth_0     :: Enet'

    , etc       :: PageAddr'
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
                         (pb_7 af_7)
                         (pb_6 af_7)

    , uart_1    = mkUART usart1
                         rcu_usart1
                         usart1_irqn
                         rcu_dma0
                         dma0
                         dma_ch6
                         dma_subperi4
                         dma0_channel6_irqn
                         (pd_6 af_7)
                         (pd_5 af_7)

    , uart_2    = mkUART usart2
                         rcu_usart2
                         usart2_irqn
                         rcu_dma0
                         dma0
                         dma_ch3
                         dma_subperi4
                         dma0_channel3_irqn
                         (pd_9 af_7)
                         (pd_8 af_7)

    , uart_3    = mkUART uart3
                         rcu_uart3
                         uart3_irqn
                         rcu_dma0
                         dma0
                         dma_ch4
                         dma_subperi4
                         dma0_channel4_irqn
                         (pc_11 af_8)
                         (pc_10 af_8)

    , uart_5    = mkUART usart5
                         rcu_usart5
                         usart5_irqn
                         rcu_dma1
                         dma1
                         dma_ch6
                         dma_subperi5
                         dma1_channel6_irqn
                         (pc_7 af_8)
                         (pc_6 af_8)

    , uart_7    = mkUART uart7
                         rcu_uart7
                         uart7_irqn
                         rcu_dma0
                         dma0
                         dma_ch0
                         dma_subperi5
                         dma0_channel0_irqn
                         (pe_0 af_8)
                         (pe_1 af_8)


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

    , in_pc_0   = mkInput pc_0
    , in_pc_1   = mkInput pc_1
    , in_pc_2   = mkInput pc_2
    , in_pc_3   = mkInput pc_3
    , in_pc_4   = mkInput pc_4
    , in_pc_5   = mkInput pc_5
    , in_pc_6   = mkInput pc_6
    , in_pc_7   = mkInput pc_7
    , in_pc_8   = mkInput pc_8
    , in_pc_9   = mkInput pc_9
    , in_pc_10  = mkInput pc_10
    , in_pc_11  = mkInput pc_11
    , in_pc_12  = mkInput pc_12
    , in_pc_13  = mkInput pc_13
    , in_pc_14  = mkInput pc_14
    , in_pc_15  = mkInput pc_15

    , in_pd_0   = mkInput pd_0
    , in_pd_1   = mkInput pd_1
    , in_pd_2   = mkInput pd_2
    , in_pd_3   = mkInput pd_3
    , in_pd_4   = mkInput pd_4
    , in_pd_5   = mkInput pd_5
    , in_pd_6   = mkInput pd_6
    , in_pd_7   = mkInput pd_7
    , in_pd_8   = mkInput pd_8
    , in_pd_9   = mkInput pd_9
    , in_pd_10  = mkInput pd_10
    , in_pd_11  = mkInput pd_11
    , in_pd_12  = mkInput pd_12
    , in_pd_13  = mkInput pd_13
    , in_pd_14  = mkInput pd_14
    , in_pd_15  = mkInput pd_15

    , in_pe_0   = mkInput pe_0
    , in_pe_1   = mkInput pe_1
    , in_pe_2   = mkInput pe_2
    , in_pe_3   = mkInput pe_3
    , in_pe_4   = mkInput pe_4
    , in_pe_5   = mkInput pe_5
    , in_pe_6   = mkInput pe_6
    , in_pe_7   = mkInput pe_7
    , in_pe_8   = mkInput pe_8
    , in_pe_9   = mkInput pe_9
    , in_pe_10  = mkInput pe_10
    , in_pe_11  = mkInput pe_11
    , in_pe_12  = mkInput pe_12
    , in_pe_13  = mkInput pe_13
    , in_pe_14  = mkInput pe_14
    , in_pe_15  = mkInput pe_15

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

    , out_pc_0  = mkOutput pc_0
    , out_pc_1  = mkOutput pc_1
    , out_pc_2  = mkOutput pc_2
    , out_pc_3  = mkOutput pc_3
    , out_pc_4  = mkOutput pc_4
    , out_pc_5  = mkOutput pc_5
    , out_pc_6  = mkOutput pc_6
    , out_pc_7  = mkOutput pc_7
    , out_pc_8  = mkOutput pc_8
    , out_pc_9  = mkOutput pc_9
    , out_pc_10 = mkOutput pc_10
    , out_pc_11 = mkOutput pc_11
    , out_pc_12 = mkOutput pc_12
    , out_pc_13 = mkOutput pc_13
    , out_pc_14 = mkOutput pc_14
    , out_pc_15 = mkOutput pc_15

    , out_pd_0  = mkOutput pd_0
    , out_pd_1  = mkOutput pd_1
    , out_pd_2  = mkOutput pd_2
    , out_pd_3  = mkOutput pd_3
    , out_pd_4  = mkOutput pd_4
    , out_pd_5  = mkOutput pd_5
    , out_pd_6  = mkOutput pd_6
    , out_pd_7  = mkOutput pd_7
    , out_pd_8  = mkOutput pd_8
    , out_pd_9  = mkOutput pd_9
    , out_pd_10 = mkOutput pd_10
    , out_pd_11 = mkOutput pd_11
    , out_pd_12 = mkOutput pd_12
    , out_pd_13 = mkOutput pd_13
    , out_pd_14 = mkOutput pd_14
    , out_pd_15 = mkOutput pd_15

    , out_pe_0  = mkOutput pe_0
    , out_pe_1  = mkOutput pe_1
    , out_pe_2  = mkOutput pe_2
    , out_pe_3  = mkOutput pe_3
    , out_pe_4  = mkOutput pe_4
    , out_pe_5  = mkOutput pe_5
    , out_pe_6  = mkOutput pe_6
    , out_pe_7  = mkOutput pe_7
    , out_pe_8  = mkOutput pe_8
    , out_pe_9  = mkOutput pe_9
    , out_pe_10 = mkOutput pe_10
    , out_pe_11 = mkOutput pe_11
    , out_pe_12 = mkOutput pe_12
    , out_pe_13 = mkOutput pe_13
    , out_pe_14 = mkOutput pe_14
    , out_pe_15 = mkOutput pe_15

    , od_pb_3   = mkOpenDrain pb_3

    , timer_1   =  cfg_timer_1
    , timer_2   =  cfg_timer_2
    , timer_3   =  cfg_timer_3
    , timer_6   =  cfg_timer_6
    , timer_7   =  cfg_timer_7

    , pwm_0     = mkPWM cfg_timer_3
                        timer_ch_0
                        (pd_12 af_2)
    , pwm_1     = mkPWM cfg_timer_3
                        timer_ch_1
                        (pd_13 af_2)
    , pwm_2     = mkPWM cfg_timer_3
                        timer_ch_2
                        (pd_14 af_2)
    , pwm_3     = mkPWM cfg_timer_3
                        timer_ch_3
                        (pd_15 af_2)


    , npx_pwm_0 = mkNeoPixelPWM cfg_timer_2
                                timer_ch_2 rcu_dma0
                                dma0 dma_ch2
                                dma_subperi5
                                dma0_channel2_irqn ch2cv
                                (pb_0 af_2)

    , npx_pwm_1 = mkNeoPixelPWM cfg_timer_7
                                timer_ch_3 rcu_dma1
                                dma1 dma_ch1
                                dma_subperi7
                                dma1_channel1_irqn ch3cv
                                (pc_9 af_3)


    , ow_0  = mkOneWire cfg_timer_6

    , eth_0 = mkENET  (pa_1  af_11)
                      (pa_2  af_11)
                      (pc_1  af_11)
                      (pa_7  af_11)
                      (pc_4  af_11)
                      (pc_5  af_11)
                      (pb_11 af_11)
                      (pb_12 af_11)
                      (pb_13 af_11)
                      enet_irqn

    , etc = mkPage 0x808_0000 fmc_sector_8

    }



gd32f450vgt6 :: MCUmod GD32F4xx
gd32f450vgt6 = gd32f4xx "gd32f450" "vgt6"

gd32f450vit6 :: MCUmod GD32F4xx
gd32f450vit6 = gd32f4xx "gd32f450" "vit6"


instance Pull GD32F4xx GPIO_PUPD where
    pullNone _ = gpio_pupd_none
    pullUp   _ = gpio_pupd_pullup
    pullDown _ = gpio_pupd_pulldown
