{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Core.Task where

import           Ivory.Language


data Task = Task
    { period  :: Maybe Uint32
    , runTask :: Def ('[] :-> ())
    }


task :: Maybe Uint32
     -> String
     -> (forall s. Ivory (ProcEffects s ()) ())
     -> Task
task p id b = Task
    { period    = p
    , runTask = proc (id <> "_task") $ body b
    }


delay :: Uint32
      -> String
      -> (forall s. Ivory (ProcEffects s ()) ())
      -> Task
delay p = task (Just p)


yeld :: String
     -> (forall s. Ivory (ProcEffects s ()) ())
     -> Task
yeld = task Nothing
