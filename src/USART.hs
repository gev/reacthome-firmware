{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module USART (usart) where

import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Stdlib
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.USART


usart :: ModuleM ()
usart = do
  inclRCU
  inclGPIO
  inclUSART
  incl main
  incl initialize
  incl loop


main :: Def ('[] :-> Sint32)
main = proc "main" $ body $ do
  call_ initialize
  call_ loop
  ret 0

initialize :: Def('[] :-> ())
initialize = proc "initialize" $ body $ do
  enablePeriphClock RCU_GPIOA
  setAF             GPIOA   GPIO_AF_1                           GPIO_PIN_2
  setMode           GPIOA   GPIO_MODE_AF    GPIO_PUPD_PULLUP    GPIO_PIN_2
  setOutputOptions  GPIOA   GPIO_OTYPE_PP   GPIO_OSPEED_50MHZ   GPIO_PIN_2
  setAF             GPIOA   GPIO_AF_1                           GPIO_PIN_3
  setMode           GPIOA   GPIO_MODE_AF    GPIO_PUPD_PULLUP    GPIO_PIN_3
  setOutputOptions  GPIOA   GPIO_OTYPE_PP   GPIO_OSPEED_50MHZ   GPIO_PIN_3
  enablePeriphClock RCU_USART1
  deinitUSART       USART1
  setWordLength     USART1  USART_WL_8BIT
  setStopBit        USART1  USART_STB_1BIT
  configParity      USART1  USART_PM_NONE
  setBaudrate       USART1  1_000_000
  configReceive     USART1  USART_RECEIVE_ENABLE
  configTransmit    USART1  USART_TRANSMIT_ENABLE
  enableUSART       USART1

loop :: Def('[] :-> ())
loop = proc "loop" $ body $ do
  forever $ do
    isDataRecived <- getFlag USART1 USART_FLAG_RBNE
    when isDataRecived $ do
      b <- receiveData USART1
      forever $ do
        isBusy <- getFlag USART1 USART_FLAG_TBE
        unless isBusy breakOut
        transmitData USART1 b
