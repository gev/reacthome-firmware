{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Concurrent.Queue where

import           Data.Concurrent.Atomically
import           Data.Concurrent.Semaphore
import           Data.Queue                 as Q
import           Data.Semaphore
import           GHC.TypeNats
import           Ivory.Language


{--
    TODO: Make polymorph?
--}
data Queue (n :: Nat) = Queue
    { producerIx :: Index Uint16
    , consumerIx :: Index Uint16
    , producerS  :: Semaphore Uint32
    , consumerS  :: Semaphore Uint32
    }


queue :: forall m n. (MonadState Context m, KnownNat n)
      => String -> m (Queue n)
queue id = do
    let name        = id    <>  "_queue"
    let producerId  = name  <>  "_queue_producer"
    let consumerId  = name  <>  "_queue_consumer"
    producerIx     <- index     producerId
    consumerIx     <- index     consumerId
    producerS      <- semaphore producerId $ fromIntegral $ fromTypeNat (aNat :: NatType n)
    consumerS      <- semaphore consumerId 0
    pure Queue { producerIx, consumerIx, producerS, consumerS }


push :: Queue n -> (Uint16 -> Ivory eff ()) -> Ivory eff ()
push Queue{..} handle =
    down producerS $ do
        x <- deref producerIx
        store producerIx $ x + 1
        handle x
        upConcurrently consumerS


pushConcurrently' :: Queue n -> (Uint16 -> Ivory eff ()) -> Ivory eff () -> Ivory eff ()
pushConcurrently' Queue{..} handle =
    downConcurrently' producerS $ do
        x <- deref producerIx
        store producerIx $ x + 1
        handle x
        upConcurrently consumerS


popConcurrently :: Queue n -> (Uint16 -> Ivory eff ()) -> Ivory eff ()
popConcurrently Queue{..} handle =
    downConcurrently consumerS $ do
        x <- deref consumerIx
        store consumerIx $ x + 1
        handle x
        upConcurrently producerS


popConcurrently' :: Queue n -> (Uint16 -> Ivory eff ()) -> Ivory eff () -> Ivory eff ()
popConcurrently' Queue{..} handle =
     downConcurrently' consumerS $ do
        x <- deref consumerIx
        store consumerIx $ x + 1
        handle x
        upConcurrently producerS


clearConcurrently :: forall n eff. KnownNat n => Queue n -> Ivory eff ()
clearConcurrently = atomically . Q.clear
