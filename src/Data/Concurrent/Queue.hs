{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
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
    let producerId  = name  <>  "_producer"
    let consumerId  = name  <>  "_consumer"
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
        up consumerS


push' :: Queue n -> (Uint16 -> Ivory eff ()) -> Ivory eff () -> Ivory eff ()
push' Queue{..} handle =
    down' producerS $ do
        x <- deref producerIx
        store producerIx $ x + 1
        handle x
        up consumerS


pop :: Queue n -> (Uint16 -> Ivory eff ()) -> Ivory eff ()
pop Queue{..} handle =
    down consumerS $ do
        x <- deref consumerIx
        store consumerIx $ x + 1
        handle x
        up producerS


pop' :: Queue n -> (Uint16 -> Ivory eff ()) -> Ivory eff () -> Ivory eff ()
pop' Queue{..} handle =
     down' consumerS $ do
        x <- deref consumerIx
        store consumerIx $ x + 1
        handle x
        up producerS


peek :: Queue n -> (Uint16 -> Ivory eff ()) -> Ivory eff ()
peek Queue{..} handle = 
    check consumerS $ do
        x <- deref consumerIx
        handle x


peek' :: Queue n -> (Uint16 -> Ivory eff ()) -> Ivory eff () -> Ivory eff ()
peek' Queue{..} handle = 
    check' consumerS $ do
        x <- deref consumerIx
        handle x


size :: Queue n -> Ivory eff Uint16
size Queue{..} =
    (-) <$> deref producerIx <*> deref consumerIx


remove :: Queue n -> Ivory eff ()
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
