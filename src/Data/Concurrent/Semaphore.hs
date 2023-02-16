{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Concurrent.Semaphore where

import           Core.Include
import           Data.Class
import           Data.Value
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Stdlib


newtype Semaphore t = Semaphore { getSemaphore :: Value t }


semaphore :: String -> Uint32 -> Semaphore Uint32
semaphore id = Semaphore . value (id <> "_semaphore")


up :: (IvoryStore t, Num t)
   => Semaphore t -> Ivory eff ()
up (Semaphore s) = do
    v <- getValue s
    setValue s $ v + 1

down :: (IvoryStore t, IvoryOrd t, Num t)
     => Semaphore t -> Ivory eff () -> Ivory eff ()
down (Semaphore s) run = do
    v <- getValue s
    when (v >? 0) $ do
        setValue s $ v - 1
        run


instance Include (Semaphore t) where
     include = include . getSemaphore