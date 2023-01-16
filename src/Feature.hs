{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Feature where

import           Include
import           Initialize
import           Interface.Timer
import           Ivory.Language


data Feature where
    Feature :: Task t => t -> Feature


data Step = Step
    { period  :: Maybe Uint32
    , runStep :: Def ('[] :-> ())
    }


step :: Maybe Uint32
     -> String
     -> (forall s. Ivory (ProcEffects s ()) ())
     -> Step
step p id b = Step
    { period    = p
    , runStep = proc (id <> "_step") $ body b
    }


delay :: Uint32
      -> String
      -> (forall s. Ivory (ProcEffects s ()) ())
      -> Step
delay p = step (Just p)


yeld :: String
     -> (forall s. Ivory (ProcEffects s ()) ())
     -> Step
yeld = step Nothing


class (Include t, Initialize t) => Task t where
    tasks :: t -> [Step]

instance Include Feature where
    include (Feature f) = include f

instance Initialize Feature where
    initialize (Feature f) = initialize f

instance Task Feature where
    tasks (Feature f) = tasks f
