{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}

module Util.Semaphore where

import           Include
import           Ivory.Language
import           Ivory.Stdlib   (when)


newtype Semaphore = Semaphore { value :: MemArea (Stored Uint32) }


semaphore :: Show s => s -> Uint32 -> Semaphore
semaphore name value = Semaphore
                     $ area ("semaphore_" <> show name)
                            (Just $ ival value)


instance Include Semaphore  where
   include (Semaphore v) = defMemArea v

up :: Semaphore -> Ivory eff ()
up (Semaphore value) = do
   let a = addrOf value
   v <- deref a
   store a $ v + 1

down (Semaphore value) run = do
   let a = addrOf value
   v <- deref a
   when (v >? 0) $ do
      store a $ v - 1
      run
