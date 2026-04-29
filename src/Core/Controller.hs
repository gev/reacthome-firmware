module Core.Controller where

import Data.Buffer
import GHC.TypeNats
import Ivory.Language

type OnMessage l s t =
    (KnownNat l) =>
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()

dontHandle :: OnMessage l s t
dontHandle _ _ = pure ()
