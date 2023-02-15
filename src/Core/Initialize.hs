{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Core.Initialize where

import           Ivory.Language
import           Ivory.Language.Module


class Initialize a where
    initialize :: a -> [Def ('[] :-> ())]
