{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Feature.Scheduler where

import           Feature
import qualified Interface       as I
import           Interface.IRQ   as Q
import           Interface.Timer as I
import           Ivory.Language
import           Prepare


data Scheduler a = I.Timer a => Scheduler Int a

instance Prepare (Scheduler t) where
  prepare (Scheduler n t) =
     Pack (  defMemArea clock
          :  I.dependencies t
          <> Q.dependencies t (handleIRQ clock)
          )
          [initialize' n t]
          []
     where clock = clock' n

handleIRQ :: MemArea ('Stored Uint32) -> Ivory (ProcEffects s ()) ()
handleIRQ clock = do
  let c = addrOf clock
  v <- deref c
  store c $ v + 1

initialize' :: I.Timer t => Int -> t -> Def ('[] :-> ())
initialize' n t =
  proc ("scheduler_" <> show n <> "_init") $ body $ do
    I.initialize t
    Q.initialize t

clock' :: Int -> MemArea ('Stored Uint32)
clock' n = area ("scheduler_" <> show n <> "_clock")
                (Just (ival 0))
