{-# LANGUAGE DataKinds #-}

module Util.Semaphore where

import           Include
import           Ivory.Language
import           Ivory.Stdlib


newtype Semaphore = Semaphore { value :: MemArea (Stored Uint32) }


semaphore :: String -> Uint32 -> Semaphore
semaphore id value = Semaphore
                   $ area (id <> "_semaphore")
                          (Just $ ival value)


up :: Semaphore -> Ivory eff ()
up (Semaphore value) = do
   let a = addrOf value
   v <- deref a
   store a $ v + 1

down :: Semaphore -> Ivory eff () -> Ivory eff ()
down (Semaphore value) run = do
   let a = addrOf value
   v <- deref a
   when (v >? 0) $ do
      store a $ v - 1
      run

get :: Semaphore -> Ivory eff Uint32
get (Semaphore value) = deref $ addrOf value


instance Include Semaphore  where
   include (Semaphore v) = defMemArea v
