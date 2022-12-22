{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Util.Data.Concurrent.Queue where

import           GHC.TypeNats
import           Include
import           Ivory.Language
import           Ivory.Language.Array
import           Ivory.Language.Proxy
import           Ivory.Stdlib
import           Util.Data.Buffer
import           Util.Data.Concurrent.Semaphore
import           Util.Data.Index


data Queue n = Queue
  { producerIx :: Index  n
  , consumerIx :: Index  n
  , producerS  :: Semaphore
  , consumerS  :: Semaphore
  }


queue :: forall n t. KnownNat n => String -> Queue n
queue id =
  let name       = id   <> "_queue"
      producerId = name <> "_producer"
      consumerId = name <> "_consumer"
  in Queue { producerIx  = index     producerId
           , consumerIx  = index     consumerId
           , producerS   = semaphore producerId $ fromInteger $ fromTypeNat (aNat :: NatType n)
           , consumerS   = semaphore consumerId 0
           }


process :: (IvoryStore a, Num a)
        => MemArea ('Stored a)
        -> Semaphore
        -> Semaphore
        -> (a -> Ivory eff ())
        -> Ivory eff ()
process index s cos handle =
  down s $ do
    let pIx = addrOf index
    ix <- deref pIx
    handle ix
    store pIx $ ix + 1
    up cos


push :: KnownNat n => Queue n -> (Ix n -> Ivory eff ()) -> Ivory eff ()
push (Queue {producerIx, producerS, consumerS}) =
  process producerIx producerS consumerS


pop :: KnownNat n => Queue n -> (Ix n -> Ivory eff ()) -> Ivory eff ()
pop (Queue {consumerIx, consumerS, producerS}) =
  process consumerIx consumerS producerS


size :: KnownNat n => Queue n -> Ivory eff (Ix n)
size (Queue {producerIx, consumerIx}) = do
  p <- deref $ addrOf producerIx
  c <- deref $ addrOf consumerIx
  pure $ p - c


instance KnownNat n => Include (Queue n) where
  include (Queue producerIx consumerIx producerS consumerS) = do
    include producerIx
    include consumerIx
    include producerS
    include consumerS
