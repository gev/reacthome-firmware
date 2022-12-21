{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Util.Queue where

import           GHC.TypeNats
import           Include
import           Ivory.Language
import           Ivory.Language.Array
import           Ivory.Stdlib
import           Util.Buffer
import           Util.Index
import           Util.Semaphore


data Queue n t = Queue
  { buff       :: Buffer n t
  , producerIx :: MemArea (Stored (Ix n))
  , consumerIx :: MemArea (Stored (Ix n))
  , producerS  :: Semaphore
  , consumerS  :: Semaphore
  }


queue :: (KnownNat n, IvoryType t, IvoryZeroVal t)
       => String -> Queue n t
queue id =
  let name       = id   <> "_queue"
      producerId = name <> "_producer"
      consumerId = name <> "_consumer"
      b = buffer name
  in Queue { buff        = b
           , producerIx  = index     producerId
           , consumerIx  = index     consumerId
           , producerS   = semaphore producerId $ arrayLen $ addrOf b
           , consumerS   = semaphore consumerId 0
           }


push :: (IvoryStore t, KnownNat n)
      => Queue n t -> t -> Ivory eff ()
push (Queue {buff, producerIx, producerS, consumerS}) v =
  down producerS $ do
    let a = addrOf buff
    let pIx = addrOf producerIx
    ix <- deref pIx
    store (a ! ix) v
    store pIx $ ix + 1
    up consumerS


pop :: (IvoryStore t, KnownNat n)
     => Queue n t -> (t -> Ivory eff ()) -> Ivory eff ()
pop (Queue {buff, consumerIx, consumerS, producerS}) run =
  down consumerS $ do
    let a = addrOf buff
    let cIx = addrOf consumerIx
    ix <- deref cIx
    v <- deref (a ! ix)
    store cIx $ ix + 1
    up producerS
    run v


size :: KnownNat n => Queue n t -> Ivory eff (Ix n)
size (Queue {producerIx, consumerIx}) = do
  p <- deref $ addrOf producerIx
  c <- deref $ addrOf consumerIx
  pure $ p - c


instance (KnownNat n, IvoryType t) => Include (Queue n t) where
  include (Queue buffer producerIx consumerIx producerS consumerS) = do
    include buffer
    include producerIx
    include consumerIx
    include producerS
    include consumerS
