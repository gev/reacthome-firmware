{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Concurrent.Semaphore where

import           Core.Context
import           Data.Value
import           Ivory.Language
import           Ivory.Stdlib


newtype Semaphore t = Semaphore { getSemaphore :: Value t }


semaphore :: String -> Uint32 -> Semaphore Uint32
semaphore id = Semaphore . value (id <> "_semaphore")


up :: (IvoryStore t, Num t)
   => Semaphore t -> Ivory eff ()
up (Semaphore s) = do
    v <- deref $ addrOf s
    store (addrOf s) $ v + 1


down :: (IvoryStore t, IvoryOrd t, Num t)
     => Semaphore t -> Ivory eff () -> Ivory eff ()
down (Semaphore s) run = do
    v <- deref $ addrOf s
    when (v >? 0) $ do
        store (addrOf s) $ v - 1
        run


check :: (IvoryStore t, IvoryOrd t, Num t)
      => Semaphore t -> Ivory eff () -> Ivory eff ()
check (Semaphore s) run = do
    v <-  deref $ addrOf s
    when (v >? 0) run

instance IvoryType t => Include (Semaphore t) where
     include s = include $ getSemaphore s
