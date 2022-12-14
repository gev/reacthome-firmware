{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module Device.GD32F3x0.USART where

import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.IRQ           as D
import           Interface
import           Interface.IRQ                 as Q
import qualified Interface.USART               as I
import           Ivory.Language
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.USART as S
import Support.Device.GD32F3x0
import Support.Device.GD32F3x0.Misc


data USART = USART
  { usart :: USART_PERIPH
  , rcu   :: RCU_PERIPH
  , rx    :: PORT
  , tx    :: PORT
  }

usart_1 = USART USART1
                RCU_USART1
                (pa_3 $ AF GPIO_AF_1)
                (pa_2 $ AF GPIO_AF_1)

usart_1_irq = IRQ USART1_IRQn usart_1

instance Interface USART where

  dependencies = const $ inclUSART <> dependencies'

  initialize (USART usart rcu rx tx) =
    initialize' rx : initialize' tx : [
      proc (show usart <> "_init") $ body $ do
        enablePeriphClock rcu
        deinitUSART     usart
        configReceive   usart USART_RECEIVE_ENABLE
        configTransmit  usart USART_TRANSMIT_ENABLE
        -- setBaudrate     usart 1_000_000
        -- setWordLength   usart USART_WL_8BIT
        -- configParity    usart USART_PM_NONE
        -- enableUSART     usart
    ]


instance I.USART USART where

  {-
    TODO: Should we "deinit" USART before change a configuration?
  -}
  setBaudrate   u     = S.setBaudrate $ usart u
  setWordLength u wl  = S.setWordLength (usart u) (coerceWordLength wl)
  setStopBit    u sb  = S.setStopBit    (usart u) (coerceStopBit sb)
  setParity     u p   = S.configParity  (usart u) (coerceParity p)

  receive = S.receiveData . usart
  transmit = S.transmitData . usart

  hasReceived    u  =  getFlag (usart u) USART_FLAG_RBNE
  hasTransmitted u  =  getFlag (usart u) USART_FLAG_TC
  canTransmit    u  =  getFlag (usart u) USART_FLAG_TBE
  enable         u  = enableUSART (usart u)


{-
  TODO: add all values of word length, stopbit and parity
-}
coerceWordLength :: I.WordLength -> USART_WORD_LENGTH
coerceWordLength I.WL_8b = USART_WL_8BIT

coerceStopBit :: I.StopBit -> USART_STOP_BIT
coerceStopBit I.SB_1b = USART_STB_1BIT

coerceParity :: I.Parity -> USART_PARITY_CFG
coerceParity I.None = USART_PM_NONE



instance Interface (D.IRQ USART) where
  dependencies (IRQ {source}) =
    dependencies source <> inclG <> inclMisc
  initialize q@(IRQ {source, irq}) =
    initialize source <> [
      proc (show (usart source) <> "_irq_init") $ body $ do
        enableIrqNvic irq 0 0
        enable q
    ]

instance Q.IRQ (D.IRQ USART) where
  handleIRQ (IRQ {source = (USART {usart})}) = makeIRQHandler usart
      
  enable (IRQ {source = (USART {usart})}) = enableInterrupt usart USART_INT_RBNE
  

instance I.USART (D.IRQ USART) where
  setBaudrate (IRQ {source}) = I.setBaudrate source
  setWordLength (IRQ {source}) = I.setWordLength source
  setStopBit (IRQ {source}) = I.setStopBit source
  setParity (IRQ {source}) = I.setParity source
  receive (IRQ {source}) = I.receive source
  transmit (IRQ {source}) = I.transmit source
  hasReceived (IRQ {source}) = I.hasReceived source
  hasTransmitted (IRQ {source}) = I.hasTransmitted source
  canTransmit (IRQ {source}) = I.canTransmit source
  enable (IRQ {source}) = I.enable source
