{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Data.Concurrent.Queue where

import           Core.Include
import           Data.Class
import           Data.Concurrent.Semaphore
import           Data.Index
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Array
import           Ivory.Language.Proxy
import           Ivory.Stdlib


data Queue (n :: Nat) = Queue
    { producerIx :: Index Uint16
    , consumerIx :: Index Uint16
    , producerS  :: Semaphore Uint32
    , consumerS  :: Semaphore Uint32
    }


queue :: forall n. KnownNat n => String -> Queue n
queue id =
    let name       = id   <> "_queue"
        producerId = name <> "_producer"
        consumerId = name <> "_consumer"
    in Queue { producerIx = index     producerId
             , consumerIx = index     consumerId
             , producerS  = semaphore producerId $ fromInteger $ fromTypeNat (aNat :: NatType n)
             , consumerS  = semaphore consumerId 0
             }


run (Index ix) s cos handle =
    down s $ do
        x <- getValue ix
        handle x
        setValue ix $ x + 1
        up cos


push :: Queue n -> (Uint16 -> Ivory eff a) -> Ivory eff ()
push (Queue {producerIx, producerS, consumerS}) =
    run producerIx producerS consumerS


pop :: Queue n -> (Uint16 -> Ivory eff a) -> Ivory eff ()
pop (Queue {consumerIx, consumerS, producerS}) =
    run consumerIx consumerS producerS


size :: Queue n -> Ivory eff Uint16
size (Queue {producerIx = (Index px), consumerIx = (Index cx)}) =
    (-) <$> getValue px <*> getValue cx


instance Include (Queue n) where
    include (Queue producerIx consumerIx producerS consumerS) = do
        include producerIx
        include consumerIx
        include producerS
        include consumerS
