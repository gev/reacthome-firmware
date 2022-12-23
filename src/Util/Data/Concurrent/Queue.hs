{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Util.Data.Concurrent.Queue where

import           GHC.TypeNats
import           Include
import           Ivory.Language
import           Ivory.Language.Array
import           Ivory.Language.Proxy
import           Ivory.Stdlib
import           Util.Data.Class
import           Util.Data.Concurrent.Semaphore
import           Util.Data.Index


data Queue n = Queue
  { producerIx :: Index  n
  , consumerIx :: Index  n
  , producerS  :: Semaphore Uint32
  , consumerS  :: Semaphore Uint32
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


run (Index ix) s cos handle =
  down s $ do
    x <- getValue ix
    handle x
    setValue ix $ x + 1
    up cos


push :: KnownNat n => Queue n -> (Ix n -> Ivory eff a) -> Ivory eff ()
push (Queue {producerIx, producerS, consumerS}) =
  run producerIx producerS consumerS


pop :: KnownNat n => Queue n -> (Ix n -> Ivory eff a) -> Ivory eff ()
pop (Queue {consumerIx, consumerS, producerS}) =
  run consumerIx consumerS producerS


size :: KnownNat n => Queue n -> Ivory eff (Ix n)
size (Queue {producerIx = (Index px), consumerIx = (Index cx)}) =
  (-) <$> getValue px <*> getValue cx


instance Include (Queue n) where
  include (Queue producerIx consumerIx producerS consumerS) = do
    include producerIx
    include consumerIx
    include producerS
    include consumerS
