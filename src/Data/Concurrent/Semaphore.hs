{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Concurrent.Semaphore where

import           Control.Monad.Writer (WriterT)
import           Core.Context
import           Data.Value
import           Ivory.Language
import           Ivory.Stdlib


newtype Semaphore t = Semaphore { getSemaphore :: Value t }


semaphore :: Monad m => String -> Uint32 -> WriterT Context m (Semaphore Uint32)
semaphore id n = do
    v <- value (id <> "_semaphore") n
    pure $ Semaphore v


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
