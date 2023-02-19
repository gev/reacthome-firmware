module Core.Controller where

import           Data.Buffer
import           Ivory.Language
import           Ivory.Stdlib


class Controller c where
    handle :: c -> Buffer n t -> m -> [Cond (ProcEffects s ()) ()]
    handle _ _ _ = []
