{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Concurrent.Queue where

import           Control.Monad.State       (MonadState)
import           Core.Context
import           Data.Concurrent.Semaphore
import           Data.Index
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Array
import           Ivory.Language.Proxy
import           Ivory.Stdlib


{--
    TODO: Make polymorph?
--}
data Queue n = Queue
    { producerIx :: Index (Ix n)
    , consumerIx :: Index (Ix n)
    , producerS  :: Semaphore Uint16
    , consumerS  :: Semaphore Uint16
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


push :: KnownNat n => Queue n -> (Ix n -> Ivory eff ()) -> Ivory eff ()
push Queue{..} handle =
    down producerS $ do
        x <- deref producerIx
        store producerIx $ x + 1
        handle x
        up consumerS


push' :: KnownNat n => Queue n -> (Ix n -> Ivory eff ()) -> Ivory eff () -> Ivory eff ()
push' Queue{..} handle =
    down' producerS $ do
        x <- deref producerIx
        store producerIx $ x + 1
        handle x
        up consumerS


pop :: KnownNat n => Queue n -> (Ix n -> Ivory eff ()) -> Ivory eff ()
pop Queue{..} handle =
    down consumerS $ do
        x <- deref consumerIx
        store consumerIx $ x + 1
        handle x
        up producerS


pop' :: KnownNat n => Queue n -> (Ix n -> Ivory eff ()) -> Ivory eff () -> Ivory eff ()
pop' Queue{..} handle =
     down' consumerS $ do
        x <- deref consumerIx
        store consumerIx $ x + 1
        handle x
        up producerS


peek :: KnownNat n => Queue n -> (Ix n -> Ivory eff ()) -> Ivory eff ()
peek Queue{..} handle =
    check consumerS $ do
        x <- deref consumerIx
        handle x


peek' :: KnownNat n => Queue n -> (Ix n -> Ivory eff ()) -> Ivory eff () -> Ivory eff ()
peek' Queue{..} handle =
    check' consumerS $ do
        x <- deref consumerIx
        handle x


size :: Queue n -> Ivory eff Uint16
size Queue{..} = do
    deref $ getSemaphore producerS


remove :: KnownNat n => Queue n -> Ivory eff ()
remove Queue{..} =
    down consumerS $ do
        x <- deref consumerIx
        store consumerIx $ x + 1
        up producerS



clear :: forall n eff. KnownNat n => Queue n -> Ivory eff ()
clear Queue{..} = do
    store consumerIx 0
    store producerIx 0
    store (getSemaphore producerS) $ fromIntegral $ fromTypeNat (aNat :: NatType n)
    store (getSemaphore consumerS) 0
