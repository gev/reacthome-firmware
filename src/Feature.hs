{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Feature where

import           Include
import           Initialize
import           Interface.Timer
import           Ivory.Language

-- data Dim'
--   = AC
--   | DC

-- data AO      a = AO            Int   a
-- data Button  a = Button        Int   a
-- data ButtonL a = ButtonL Int   Int   a
-- data Dim     a = Dim     Dim'  Int   a
-- data Doppler a = Doppler       Int   a
-- data Eth     a = Eth           Int   a
-- data In      a = In            Int   a
-- data FindMe  a = FindMe        Int   a
-- data OW      a = OW            Int   a
-- data RS485   a = RS485         Int   a
-- data Service a = Service       Int   a


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
step p n b = Step
  { period  = p
  , runStep = proc (n <> "_step") $ body b
  }


class (Include t, Initialize t) => Task t where
  tasks :: t -> [Step]

instance Include Feature where
  include (Feature f) = include f

instance Initialize Feature where
  initialize (Feature f) = initialize f

instance Task Feature where
  tasks (Feature f) = tasks f
