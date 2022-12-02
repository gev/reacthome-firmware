{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Prepare where

import           Ivory.Language
import           Ivory.Language.Module


data Pack = Pack
  { dependencies :: [ModuleM ()]
  , initialize   :: [Def ('[] :-> ())]
  , step         :: [Def ('[] :-> ())]
  }

class Prepare f where
  prepare :: f -> Pack
