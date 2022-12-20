{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Interface where

import           Ivory.Language
import           Ivory.Language.Module

class Interface a where
  include  :: a -> ModuleM ()
  initialize    :: a -> [Def ('[] :-> ())]


-- class Interface a where
--   include  :: a -> [ModuleM ()]
--   initialize    :: a -> [Def ('[] :-> ())]
