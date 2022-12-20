{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Interface.USART where

import           Interface
import           Ivory.Language
import           Ivory.Language.Module

data HandleUSART u = HandleUSART
  { usart     :: u
  , onReceive :: Uint16 -> forall eff. Ivory eff ()
  , onDrain   ::           forall eff. Ivory eff ()
  }


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

class Interface (HandleUSART u) => USART u where

  setBaudrate     :: u -> Uint32     -> Ivory eff ()
  setWordLength   :: u -> WordLength -> Ivory eff ()
  setStopBit      :: u -> StopBit    -> Ivory eff ()
  setParity       :: u -> Parity     -> Ivory eff ()

  receive         :: u -> Ivory eff Uint16

  transmit        :: u -> Ref r (CArray (Stored Uint16))
                       -> Uint16
                       -> Ivory (ProcEffects s ()) ()

  enable          :: u -> Ivory eff ()
