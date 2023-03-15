{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Core.Task where

import           Ivory.Language


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
