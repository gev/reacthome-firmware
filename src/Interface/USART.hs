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

  setBaudrate     :: a -> Uint32     -> Ivory eff ()
  setWordLength   :: a -> WordLength -> Ivory eff ()
  setStopBit      :: a -> StopBit    -> Ivory eff ()
  setParity       :: a -> Parity     -> Ivory eff ()

  receive         :: a           -> Ivory eff Uint16
  transmit        :: a -> Uint16 -> Ivory eff ()

  hasReceived     :: a -> Ivory eff IBool
  hasTransmitted  :: a -> Ivory eff IBool
  canTransmit     :: a -> Ivory eff IBool

  enable          :: a -> Ivory eff ()
