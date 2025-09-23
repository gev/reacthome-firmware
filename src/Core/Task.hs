{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Core.Task where

import Ivory.Language

data Period = Period
     { interval :: Uint32
     , phase :: Uint32
     }

data Task = Task
     { period :: Maybe Period
     , getTask :: Def ('[] :-> ())
     }

instance Eq Task where
     t1 == t2 = getTask t1 == getTask t2

task ::
     Maybe Period ->
     String ->
     (forall s. Ivory (ProcEffects s ()) ()) ->
     Task
task period id run =
     Task
          { period
          , getTask = proc (id <> "_task") $ body run
          }

delayPhase ::
     Uint32 ->
     Uint32 ->
     String ->
     (forall s. Ivory (ProcEffects s ()) ()) ->
     Task
delayPhase interval phase = task . Just $ Period interval phase

delay ::
     Uint32 ->
     String ->
     (forall s. Ivory (ProcEffects s ()) ()) ->
     Task
delay interval = delayPhase interval 0

yeld ::
     String ->
     (forall s. Ivory (ProcEffects s ()) ()) ->
     Task
yeld = task Nothing

runTask :: Task -> Ivory eff ()
runTask = call_ . getTask
