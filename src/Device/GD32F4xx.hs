{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Device.GD32F4xx where

import           Control.Monad.Writer
import           Core.Context
import           Device.GD32F4xx.GPIO
import           Device.GD32F4xx.GPIO.Input
import           Device.GD32F4xx.GPIO.Output
import           Device.GD32F4xx.GPIOs.Outputs
import           Device.GD32F4xx.Mac           (makeMac)
import           Device.GD32F4xx.SystemClock   as G
import           Device.GD32F4xx.SysTick
import           Device.GD32F4xx.Timer
import           Device.GD32F4xx.USART
import           Interface.Mac                 (Mac)
import           Interface.MCU
import           Interface.SystemClock         (SystemClock)
import           Support.Device.GD32F4xx
import           Support.Device.GD32F4xx.DMA
import           Support.Device.GD32F4xx.GPIO
import           Support.Device.GD32F4xx.IRQ
import           Support.Device.GD32F4xx.RCU
import           Support.Device.GD32F4xx.USART



type USARTW  = forall m. MonadWriter Context m => m USART
type InputW  = forall m. MonadWriter Context m => m Input
type OutputW = forall m. MonadWriter Context m => m Output



data GD32F4xx = GD32F4xx
    { usart_0   :: USARTW
    , usart_1   :: USARTW
    , usart_2   :: USARTW
    , usart_5   :: USARTW
    , usart_7   :: USARTW

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
    }


gd32f4xx :: String -> String -> MCUmod GD32F4xx
gd32f4xx = MCUmod $ mkMCU G.systemClock makeMac inclGD32F4xx GD32F4xx
    { usart_0   = mkUSART USART0
                          RCU_USART0
                          USART0_IRQn
                          RCU_DMA1
                          DMA1
                          DMA_CH7
                          DMA_SUBPERI4
                          DMA1_Channel7_IRQn
                          DMA1_Channel7
                          (pa_10 $ AF GPIO_AF_7)
                          (pa_9 $ AF GPIO_AF_7)

    , usart_1   = mkUSART USART1
                          RCU_USART1
                          USART1_IRQn
                          RCU_DMA0
                          DMA0
                          DMA_CH6
                          DMA_SUBPERI4
                          DMA0_Channel6_IRQn
                          DMA0_Channel6
                          (pd_6 $ AF GPIO_AF_7)
                          (pd_5 $ AF GPIO_AF_7)

    , usart_2   = mkUSART USART2
                          RCU_USART2
                          USART2_IRQn
                          RCU_DMA0
                          DMA0
                          DMA_CH3
                          DMA_SUBPERI4
                          DMA0_Channel3_IRQn
                          DMA0_Channel3
                          (pc_11 $ AF GPIO_AF_7)
                          (pc_10 $ AF GPIO_AF_7)

    , usart_5   = mkUSART USART5
                          RCU_USART5
                          USART5_IRQn
                          RCU_DMA1
                          DMA1
                          DMA_CH6
                          DMA_SUBPERI5
                          DMA1_Channel6_IRQn
                          DMA1_Channel6
                          (pc_7 $ AF GPIO_AF_8)
                          (pc_6 $ AF GPIO_AF_8)

    , usart_7   = mkUSART UART7
                          RCU_UART7
                          UART7_IRQn
                          RCU_DMA0
                          DMA0
                          DMA_CH0
                          DMA_SUBPERI5
                          DMA0_Channel0_IRQn
                          DMA0_Channel0
                          (pe_0 $ AF GPIO_AF_8)
                          (pe_1 $ AF GPIO_AF_8)


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
    }



gd32f450vgt6 :: MCUmod GD32F4xx
gd32f450vgt6 = gd32f4xx "gd32f450" "vgt6"

gd32f450vit6 :: MCUmod GD32F4xx
gd32f450vit6 = gd32f4xx "gd32f450" "vit6"
