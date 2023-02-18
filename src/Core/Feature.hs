{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Core.Feature where

import           Core.Controller
import           Core.Include
import           Core.Initialize
import           Core.Task
import           Interface.Timer
import           Ivory.Language


data Feature where
    Feature :: (Task f, Controller f) => f -> Feature



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



instance Include Feature where
    include (Feature f) = include f

instance Initialize Feature where
    initialize (Feature f) = initialize f

instance Task Feature where
    tasks (Feature f) = tasks f
