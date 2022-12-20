{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Util.Semaphore where

import           Ivory.Language


data Semaphore v where
  Semaphore :: (IvoryInit v, IvoryZeroVal v)
            => { value :: MemArea (Stored v)
               }
            -> Semaphore v


semaphore :: (Show s, IvoryInit v, IvoryZeroVal v) => s -> v ->Semaphore v
semaphore name value = Semaphore $ area ("semaphore_" <> show name)
                                        (Just $ ival value)

-- incl :: Semaphore -> ModuleDef
-- incl (Semaphore v) = defMemArea v

-- s = semaphore "1" 1 :: Semaphore Uint16

-- dep = defMemArea s

-- instance MemArea (Semaphore Uint32) where
