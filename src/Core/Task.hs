{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Core.Task where

import           Ivory.Language


data Task = Task
    { period  :: Maybe Uint32
    , getTask :: Def ('[] :-> ())
    }



instance Eq Task where
  t1 == t2 = getTask t1 == getTask t2



task :: Maybe Uint32
     -> String
     -> (forall s. Ivory (ProcEffects s ()) ())
     -> Task
task period id run = Task { period  = period
                          , getTask = proc (id <> "_task") $ body run
                          }


delay :: Uint32
      -> String
      -> (forall s. Ivory (ProcEffects s ()) ())
      -> Task
delay period = task (Just period)


yeld :: String
     -> (forall s. Ivory (ProcEffects s ()) ())
     -> Task
yeld = task Nothing


runTask :: Task -> Ivory eff ()
runTask = call_ . getTask
