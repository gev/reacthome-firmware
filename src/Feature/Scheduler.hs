{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Feature.Scheduler where

import           Feature
import qualified Interface       as I
import           Interface.IRQ   as Q
import           Interface.Timer as I
import           Ivory.Language


data Scheduler a = I.Timer a => Scheduler Int a

instance Prepare (Scheduler t) where
  prepare (Scheduler n t) =
     Pack (I.dependencies t <> Q.dependencies t handleIRQ)
          (initialize' n t)
          (step' n)

handleIRQ :: Ivory (ProcEffects s ()) ()
handleIRQ = retVoid

initialize' :: I.Timer t => Int -> t -> Def ('[] :-> ())
initialize' n t =
  proc ("scheduler_" <> show n <> "_init") $ body $ do
    I.initialize t
    Q.initialize t

step' :: Int -> Def ('[] ':-> ())
step' n =
  proc ("scheduler_" <> show n <> "_step") $ body retVoid
