module Device.GD32F3x0 where

import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.Mac           (makeMac)
import           Device.GD32F3x0.SystemClock   as G
import           Device.GD32F3x0.SysTick
import           Device.GD32F3x0.Timer
import           Device.GD32F3x0.USART
import           Interface.Mac                 (Mac)
import           Interface.MCU
import           Interface.SystemClock         (SystemClock)
import           Support.Device.GD32F3x0
import           Support.Device.GD32F3x0.DMA
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.USART


data GD32F3x0 = GD32F3x0
    { usart_1   :: USART

    , in_pa_0   :: In
    , in_pa_1   :: In
    , in_pa_2   :: In
    , in_pa_3   :: In
    , in_pa_4   :: In
    , in_pa_5   :: In
    , in_pa_6   :: In
    , in_pa_7   :: In
    , in_pa_8   :: In
    , in_pa_9   :: In
    , in_pa_10  :: In
    , in_pa_11  :: In
    , in_pa_12  :: In
    , in_pa_13  :: In
    , in_pa_14  :: In
    , in_pa_15  :: In

    , in_pb_0   :: In
    , in_pb_1   :: In
    , in_pb_2   :: In
    , in_pb_3   :: In
    , in_pb_4   :: In
    , in_pb_5   :: In
    , in_pb_6   :: In
    , in_pb_7   :: In
    , in_pb_8   :: In
    , in_pb_9   :: In
    , in_pb_10  :: In
    , in_pb_11  :: In
    , in_pb_12  :: In
    , in_pb_13  :: In
    , in_pb_14  :: In
    , in_pb_15  :: In

    , out_pa_0  :: Out
    , out_pa_1  :: Out
    , out_pa_2  :: Out
    , out_pa_3  :: Out
    , out_pa_4  :: Out
    , out_pa_5  :: Out
    , out_pa_6  :: Out
    , out_pa_7  :: Out
    , out_pa_8  :: Out
    , out_pa_9  :: Out
    , out_pa_10 :: Out
    , out_pa_11 :: Out
    , out_pa_12 :: Out
    , out_pa_13 :: Out
    , out_pa_14 :: Out
    , out_pa_15 :: Out

    , out_pb_0  :: Out
    , out_pb_1  :: Out
    , out_pb_2  :: Out
    , out_pb_3  :: Out
    , out_pb_4  :: Out
    , out_pb_5  :: Out
    , out_pb_6  :: Out
    , out_pb_7  :: Out
    , out_pb_8  :: Out
    , out_pb_9  :: Out
    , out_pb_10 :: Out
    , out_pb_11 :: Out
    , out_pb_12 :: Out
    , out_pb_13 :: Out
    , out_pb_14 :: Out
    , out_pb_15 :: Out
    }


gd32f3x0 = GD32F3x0
    { usart_1   = USART USART1
                        RCU_USART1
                        USART1_IRQn
                        DMA_CH3
                        DMA_Channel3_4_IRQn
                        DMA_Channel3_4
                        (pa_3 $ AF GPIO_AF_1)
                        (pa_2 $ AF GPIO_AF_1)

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
    }



instance MCU GD32F3x0 where
    mac         _ = makeMac
    systemClock _ = G.systemClock
