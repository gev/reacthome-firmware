{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Interface.IRQ where

import           Interface
import           Ivory.Language
import           Ivory.Language.Module


class Interface q => IRQ q where
  irq    :: q -> (forall s. Ivory (ProcEffects s ()) ()) -> Def ('[] :-> ())
  enable :: q -> Ivory eff ()
