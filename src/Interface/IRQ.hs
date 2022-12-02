{-# LANGUAGE RankNTypes #-}

module Interface.IRQ where

import           Interface
import           Ivory.Language
import           Ivory.Language.Module


class IRQ a where
  dependencies  :: a -> (forall s. Ivory (ProcEffects s ()) ()) -> [ModuleM ()]
  initialize    :: a -> Ivory eff ()
  enable        :: a -> Ivory eff ()
