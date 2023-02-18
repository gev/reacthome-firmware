{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Core.Task where

import           Core.Include
import           Core.Initialize
import           Ivory.Language


data Step = Step
    { period  :: Maybe Uint32
    , runStep :: Def ('[] :-> ())
    }


class (Include t, Initialize t) => Task t where
    tasks :: t -> [Step]
