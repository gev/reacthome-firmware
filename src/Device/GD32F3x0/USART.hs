{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}

module Device.GD32F3x0.USART where

import           Device.GD32F3x0.GPIO
import           Interface
import           Interface.USART               (HandleUSART)
import qualified Interface.USART               as I
import           Ivory.Language
import           Ivory.Stdlib
import           Support.Device.GD32F3x0
import           Support.Device.GD32F3x0.GPIO  (GPIO_AF (GPIO_AF_1))
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.USART as S


data USART = USART
  { usart :: USART_PERIPH
  , rcu   :: RCU_PERIPH
  , irq   :: IRQn
  , rx    :: PORT
  , tx    :: PORT
  }

usart_1 = USART USART1
                RCU_USART1
                USART1_IRQn
                (pa_3 $ AF GPIO_AF_1)
                (pa_2 $ AF GPIO_AF_1)

instance Interface (I.HandleUSART USART) where

  dependencies (I.HandleUSART (USART {usart}) onReceive onDrain) =
      makeIRQHandler usart (handleIRQ usart onReceive onDrain)
        : inclG <> inclMisc <> inclUSART <> dependencies'

  initialize (I.HandleUSART {I.usart = (USART usart rcu irq rx tx)}) =
    initialize' rx : initialize' tx : [
      proc (show usart <> "_init") $ body $ do
        enableIrqNvic     irq 0 0
        enablePeriphClock rcu
        deinitUSART     usart
        configReceive   usart USART_RECEIVE_ENABLE
        configTransmit  usart USART_TRANSMIT_ENABLE
        enableInterrupt usart USART_INT_RBNE
        enableInterrupt usart USART_INT_TC
        -- setBaudrate     usart 1_000_000
        -- setWordLength   usart USART_WL_8BIT
        -- configParity    usart USART_PM_NONE
        -- enableUSART     usart
    ]

handleIRQ :: USART_PERIPH -> (Uint16 -> Ivory eff ()) -> Ivory eff () -> Ivory eff ()
handleIRQ usart onReceive onDrain = do
  rbne <- getInterruptFlag usart USART_INT_FLAG_RBNE
  when rbne $ onReceive =<< S.receiveData usart
  tc <- getInterruptFlag usart USART_INT_FLAG_TC
  when tc onDrain


instance I.USART USART where

  {-
    TODO: Should we "deinit" USART before change a configuration?
  -}
  setBaudrate   u     = S.setBaudrate $ usart u
  setWordLength u wl  = S.setWordLength (usart u) (coerceWordLength wl)
  setStopBit    u sb  = S.setStopBit    (usart u) (coerceStopBit sb)
  setParity     u p   = S.configParity  (usart u) (coerceParity p)

  receive = S.receiveData . usart

  transmit u buff n = do
        for (toIx n) $ \ix -> do
          forever $ do
            tbe <- getFlag (usart u) USART_FLAG_TBE
            when tbe breakOut
          S.transmitData (usart u) =<< deref (buff ! ix)


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
