{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

module USART (compileUSART) where

import           Ivory.Compile.C.CmdlineFrontend
import           Ivory.Language
import           Ivory.Stdlib
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.USART


compileUSART :: IO ()
compileUSART = runCompiler
  [usartModule]
  []
  initialOpts
    { outDir = Just "./build"
    , constFold = True
    }


usartModule :: Module
usartModule = package "usart" $ do
  inclRCU
  inclGPIO
  inclUSART
  incl main



main :: Def ('[] :-> Sint32)
main = proc "main" $ body $ do
  enablePeriphClock RCU_GPIOA
  enablePeriphClock RCU_USART1

  setAF GPIOA
        GPIO_AF_1
        GPIO_PIN_2
  setAF GPIOA
        GPIO_AF_1
        GPIO_PIN_3
  setMode GPIOA
          GPIO_MODE_AF
          GPIO_PUPD_PULLUP
          GPIO_PIN_2
  setOutputOptions  GPIOA
                    GPIO_OTYPE_PP
                    GPIO_OSPEED_50MHZ
                    GPIO_PIN_2
  setMode GPIOA
          GPIO_MODE_AF
          GPIO_PUPD_PULLUP
          GPIO_PIN_3
  setOutputOptions  GPIOA
                    GPIO_OTYPE_PP
                    GPIO_OSPEED_50MHZ
                    GPIO_PIN_3
  deinitUSART USART1
  wordLengthUSART USART1 USART_WL_8BIT
  stopBitSetUSART USART1 USART_STB_1BIT
  parityConfigUSART USART1 USART_PM_NONE
  baudrateSetUSART USART1 1_000_000
  receiveConfigUSART USART1 USART_RECEIVE_ENABLE
  transmitConfigUSART USART1 USART_TRANSMIT_ENABLE
  enableUSART USART1
  forever $ do
    flag <- getFlag USART1 USART_FLAG_RBNE
    when flag $ receiveData USART1 >>= transmitData USART1


  ret 0


delay :: Def ('[Ix 1_000_000_000] :-> ())
delay = proc "delay" $ \n -> body $ do
  n `times` const (call_ nop)

nop :: Def ('[] :-> ())
nop = proc "nop" $ body retVoid
