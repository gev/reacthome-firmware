{-# LANGUAGE RankNTypes #-}

module Interface.IRQ where

import           Interface
import           Ivory.Language
import           Ivory.Language.Module

class Interface q => IRQ q where
  handleIRQ :: q -> (forall s. Ivory (ProcEffects s ()) ()) -> ModuleM ()
  enable    :: q -> Ivory eff ()
