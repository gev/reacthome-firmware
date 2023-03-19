{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Concurrent.Semaphore where

import           Control.Monad.Writer (MonadWriter)
import           Core.Context
import           Data.Value
import           Ivory.Language
import           Ivory.Stdlib


newtype Semaphore t = Semaphore { getSemaphore :: Value t }


semaphore :: MonadWriter Context m => String -> Uint32 -> m (Semaphore Uint32)
semaphore id n = do
    v <- value (id <> "_semaphore") n
    pure $ Semaphore v


up :: (IvoryStore t, Num t)
   => Semaphore t -> Ivory eff ()
up (Semaphore s) = do
    v <- deref s
    store s $ v + 1


down :: (IvoryStore t, IvoryOrd t, Num t)
     => Semaphore t -> Ivory eff () -> Ivory eff ()
down (Semaphore s) run = do
    v <- deref s
    when (v >? 0) $ do
        store s $ v - 1
        run


check :: (IvoryStore t, IvoryOrd t, Num t)
      => Semaphore t -> Ivory eff () -> Ivory eff ()
check (Semaphore s) run = do
    v <-  deref s
    when (v >? 0) run
