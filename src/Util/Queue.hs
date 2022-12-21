{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Util.Queue where

import           GHC.TypeNats
import           Include
import           Ivory.Language
import           Ivory.Language.Array
import           Ivory.Language.Proxy
import           Ivory.Stdlib
import           Util.Buffer
import           Util.Index
import           Util.Semaphore


data Queue n = Queue
  { producerIx :: Index  n
  , consumerIx :: Index  n
  , producerS  :: Semaphore
  , consumerS  :: Semaphore
  }


queue :: forall n. KnownNat n => String -> Queue n
queue id =
  let name       = id   <> "_queue"
      producerId = name <> "_producer"
      consumerId = name <> "_consumer"
  in Queue { producerIx  = index     producerId
           , consumerIx  = index     consumerId
           , producerS   = semaphore producerId $ fromInteger $ fromTypeNat (aNat :: NatType n)
           , consumerS   = semaphore consumerId 0
           }


push :: KnownNat n => Queue n -> (Ix n -> Ivory eff ()) -> Ivory eff ()
push (Queue {producerIx, producerS, consumerS}) run =
  down producerS $ do
    let pIx = addrOf producerIx
    ix <- deref pIx
    store pIx $ ix + 1
    up consumerS
    run ix


pop :: KnownNat n => Queue n -> (Ix n -> Ivory eff ()) -> Ivory eff ()
pop (Queue {consumerIx, consumerS, producerS}) run =
  down consumerS $ do
    let cIx = addrOf consumerIx
    ix <- deref cIx
    store cIx $ ix + 1
    up producerS
    run ix


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
