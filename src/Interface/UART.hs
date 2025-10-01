module Interface.UART where

import Core.Handler
import Ivory.Language

data HandleUART u = HandleUART
    { uart :: u
    , onReceive :: forall eff. Ivory eff ()
    , onTransmit :: forall eff. Ivory eff ()
    , onDrain :: forall eff. Maybe (Ivory eff ())
    , onError :: forall eff. Ivory eff ()
    }

data Parity
    = None
    | Even
    | Odd

data WordLength
    = WL_8b
    | WL_9b

data StopBit
    = SB_0_5b
    | SB_1b
    | SB_1_5b
    | SB_2b

class (Handler HandleUART u) => UART u where
    configUART ::
        u ->
        Uint32 ->
        WordLength ->
        StopBit ->
        Parity ->
        Ivory eff ()

    clearRX ::
        u ->
        Ivory eff ()

    receive ::
        u ->
        (Uint16 -> Ivory (ProcEffects s t) ()) ->
        Ivory (ProcEffects s t) ()

    transmit ::
        u ->
        ((Uint16 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s t) ()) ->
        Ivory (ProcEffects s t) ()

    enable ::
        u ->
        Ivory eff ()
