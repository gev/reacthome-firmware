{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

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
    { usart_1   :: USARTW

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
    }


gd32f4xx :: MonadWriter Context m => String -> String -> m (MCU GD32F4xx)
gd32f4xx = mcu "GD32F4xx" False G.systemClock makeMac inclGD32F4xx GD32F4xx
    { usart_1   = mkUSART USART1
                          RCU_USART1
                          USART1_IRQn
                          DMA0
                          DMA_CH1
                          DMA0_Channel0_IRQn
                          DMA0_Channel0
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



gd32f450vgt6 :: MonadWriter Context m => m (MCU GD32F4xx)
gd32f450vgt6 = gd32f4xx "GD32F4xx" "vgt6"

gd32f450vit6 :: MonadWriter Context m => m (MCU GD32F4xx)
gd32f450vit6 = gd32f4xx "GD32F4xx" "vit6"
