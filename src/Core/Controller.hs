module Core.Controller where

import Data.Buffer
import GHC.TypeNats (Nat)
import Ivory.Language

type OnMessage (l :: Nat) =
    forall s t.
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()

dontHandle :: OnMessage l
dontHandle _ _ = pure ()
