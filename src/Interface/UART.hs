{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Interface.UART where

import           Core.Context
import           Core.Handler
import           Ivory.Language
import           Ivory.Language.Module


data HandleUART u = HandleUART
    { uart       :: u
    , onReceive  :: Uint16 -> forall eff. Ivory eff ()
    , onTransmit :: forall eff. Ivory eff ()
    , onDrain    :: forall eff. Maybe (Ivory eff ())
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

class Handler HandleUART u => UART u where

    setBaudrate   :: u -> Uint32     -> Ivory eff ()
    setWordLength :: u -> WordLength -> Ivory eff ()
    setStopBit    :: u -> StopBit    -> Ivory eff ()
    setParity     :: u -> Parity     -> Ivory eff ()

    transmit      :: u -> Ref r (CArray (Stored Uint16))
                       -> Uint16
                       -> Ivory (ProcEffects s ()) ()

    enable        :: u -> Ivory eff ()
