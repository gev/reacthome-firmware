module Core.Controller where

import Data.Buffer
import GHC.TypeNats
import Ivory.Language

class Controller c where
    handle ::
        (KnownNat l) =>
        c ->
        Buffer l Uint8 ->
        Uint8 ->
        Ivory (ProcEffects s t) ()
    handle _ _ _ = pure ()

instance Controller ()
