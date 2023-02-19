module Core.Controller where

import           Data.Buffer
import           Ivory.Language
import           Ivory.Stdlib


class Controller c where
    handle :: c -> Buffer n t -> [Cond (ProcEffects s ()) ()]
    handle _ _ = []
