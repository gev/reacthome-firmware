{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Interface.USART where

import           Interface
import           Ivory.Language
import           Ivory.Language.Module

type OnReceive = Uint16 -> forall s. Ivory (ProcEffects s ()) ()

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

  receive         :: a -> Ivory eff Uint16

  transmit        :: a -> Ref r (Array 512 (Stored Uint16))
                       -> Uint16
                       -> Ivory (ProcEffects s ()) ()

  enable          :: a -> Ivory eff ()
