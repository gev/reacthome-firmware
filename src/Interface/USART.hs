module Interface.USART where

import           Interface
import           Ivory.Language
import           Ivory.Language.Module


data Parity
  = None
  | Even
  | Odd

data WordLength
  = WL_8b
  | WL_9b

data StopBit

  = SB_1b
  | SB_1_5b
  | SB_2b

class Interface a => USART a where
  setBaudrate :: a -> Uint32 -> Ivory eff ()
  setWordLength :: a -> WordLength -> Ivory eff ()
  setStopBit :: a -> StopBit -> Ivory eff ()
  setParity :: a -> Parity -> Ivory eff ()
  receiveData :: a -> Ivory eff Uint16
  transmitData :: a -> Uint16 -> Ivory eff ()
