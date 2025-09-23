module Data.Queue where

import Control.Monad.State (MonadState)
import Core.Context
import Data.Index
import Data.Semaphore
import GHC.TypeNats
import Ivory.Language
import Ivory.Language.Array
import Ivory.Language.Proxy
import Ivory.Stdlib

{--
    TODO: Make polymorph?
--}
data Queue n t = Queue
    { producerIx :: Index (Ix n)
    , consumerIx :: Index (Ix n)
    , producerS :: Semaphore Uint16
    , consumerS :: Semaphore Uint16
    , it :: t
    }

queue ::
    forall m n t.
    (MonadState Context m, KnownNat n) =>
    String ->
    t ->
    m (Queue n t)
queue id it = do
    let name = id <> "_queue"
    let producerId = name <> "_producer"
    let consumerId = name <> "_consumer"
    producerIx <- index producerId
    consumerIx <- index consumerId
    producerS <- semaphore producerId $ fromIntegral $ fromTypeNat (aNat :: NatType n)
    consumerS <- semaphore consumerId 0
    pure Queue{producerIx, consumerIx, producerS, consumerS, it}

push :: (KnownNat n) => Queue n t -> (t -> Ix n -> Ivory eff ()) -> Ivory eff ()
push Queue{..} handle =
    down producerS do
        x <- deref producerIx
        store producerIx $ x + 1
        handle it x
        up consumerS

push' ::
    (KnownNat n) =>
    Queue n t ->
    (t -> Ix n -> Ivory eff ()) ->
    Ivory eff () ->
    Ivory eff ()
push' Queue{..} handle =
    down' producerS do
        x <- deref producerIx
        store producerIx $ x + 1
        handle it x
        up consumerS

pop ::
    (KnownNat n) =>
    Queue n t ->
    (t -> Ix n -> Ivory eff ()) ->
    Ivory eff ()
pop Queue{..} handle =
    down consumerS do
        x <- deref consumerIx
        store consumerIx $ x + 1
        handle it x
        up producerS

pop' ::
    (KnownNat n) =>
    Queue n t ->
    (t -> Ix n -> Ivory eff ()) ->
    Ivory eff () ->
    Ivory eff ()
pop' Queue{..} handle =
    down' consumerS do
        x <- deref consumerIx
        store consumerIx $ x + 1
        handle it x
        up producerS

peek ::
    (KnownNat n) =>
    Queue n t ->
    (t -> Ix n -> Ivory eff ()) ->
    Ivory eff ()
peek Queue{..} handle =
    check consumerS do
        x <- deref consumerIx
        handle it x

peek' ::
    (KnownNat n) =>
    Queue n t ->
    (t -> Ix n -> Ivory eff ()) ->
    Ivory eff () ->
    Ivory eff ()
peek' Queue{..} handle =
    check' consumerS do
        x <- deref consumerIx
        handle it x

size :: Queue n t -> Ivory eff Uint16
size Queue{..} =
    deref $ getSemaphore consumerS

remove ::
    (KnownNat n) =>
    Queue n t ->
    Ivory eff ()
remove Queue{..} =
    down consumerS do
        x <- deref consumerIx
        store consumerIx $ x + 1
        up producerS

clear ::
    forall n t eff.
    (KnownNat n) =>
    Queue n t ->
    Ivory eff ()
clear Queue{..} = do
    store consumerIx 0
    store producerIx 0
    store (getSemaphore producerS) $ fromIntegral $ fromTypeNat (aNat :: NatType n)
    store (getSemaphore consumerS) 0
