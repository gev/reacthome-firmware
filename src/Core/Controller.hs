module Core.Controller where

import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib


class Controller c where
    handle :: (KnownNat n, IvoryStore t, IvoryEq t, IvoryOrd t, Num t)
           => c
           -> Buffer n t
           -> m
           -> Ivory (ProcEffects s ()) [Cond (ProcEffects s ()) ()]
    handle _ _ _ = pure []
