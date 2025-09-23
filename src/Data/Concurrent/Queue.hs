module Data.Concurrent.Queue where

import Data.Concurrent.Atomically
import Data.Concurrent.Semaphore
import Data.Queue as Q
import Data.Semaphore
import GHC.TypeNats
import Ivory.Language

pushConcurrently ::
    (KnownNat n) =>
    Queue n t ->
    (t -> Ix n -> Ivory eff ()) ->
    Ivory eff ()
pushConcurrently Queue{..} handle =
    downConcurrently producerS do
        x <- deref producerIx
        store producerIx $ x + 1
        handle it x
        upConcurrently consumerS

pushConcurrently' ::
    (KnownNat n) =>
    Queue n t ->
    (t -> Ix n -> Ivory eff ()) ->
    Ivory eff () ->
    Ivory eff ()
pushConcurrently' Queue{..} handle =
    downConcurrently' producerS do
        x <- deref producerIx
        store producerIx $ x + 1
        handle it x
        upConcurrently consumerS

popConcurrently ::
    (KnownNat n) =>
    Queue n t ->
    (t -> Ix n -> Ivory eff ()) ->
    Ivory eff ()
popConcurrently Queue{..} handle =
    downConcurrently consumerS do
        x <- deref consumerIx
        store consumerIx $ x + 1
        handle it x
        upConcurrently producerS

popConcurrently' ::
    (KnownNat n) =>
    Queue n t ->
    (t -> Ix n -> Ivory eff ()) ->
    Ivory eff () ->
    Ivory eff ()
popConcurrently' Queue{..} handle =
    downConcurrently' consumerS do
        x <- deref consumerIx
        store consumerIx $ x + 1
        handle it x
        upConcurrently producerS

removeConcurrently ::
    (KnownNat n) =>
    Queue n t ->
    Ivory eff ()
removeConcurrently Queue{..} =
    downConcurrently consumerS do
        x <- deref consumerIx
        store consumerIx $ x + 1
        up producerS

clearConcurrently ::
    forall n t eff.
    (KnownNat n) =>
    Queue n t ->
    Ivory eff ()
clearConcurrently = atomically . Q.clear
