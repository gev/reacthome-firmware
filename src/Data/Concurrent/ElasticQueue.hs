module Data.Concurrent.ElasticQueue where

import Control.Monad.State (MonadState)
import Core.Context
import Data.Concurrent.Semaphore
import Data.ElasticQueue
import Data.Index
import Data.Semaphore
import Data.Value
import GHC.TypeNats
import Ivory.Language
import Ivory.Language.Array
import Ivory.Language.Proxy
import Ivory.Stdlib

pushConcurrently ::
    (KnownNat n) =>
    ElasticQueue n t ->
    (t -> Ix n -> Ivory eff ()) ->
    Ivory eff ()
pushConcurrently q@ElasticQueue{..} handle = do
    downConcurrently producerS do
        x <- deref producerIx
        store producerIx $ x + 1
        handle it x
        upConcurrently consumerS
        s <- size q
        when (s >=? half) do
            store isReady true

pushConcurrently' ::
    (KnownNat n) =>
    ElasticQueue n t ->
    (t -> Ix n -> Ivory eff ()) ->
    Ivory eff () ->
    Ivory eff ()
pushConcurrently' q@ElasticQueue{..} handleT handleF = do
    downConcurrently'
        producerS
        do
            x <- deref producerIx
            store producerIx $ x + 1
            handleT it x
            upConcurrently consumerS
            s <- size q
            when (s >=? half) do
                store isReady true
        handleF

popConcurrently ::
    (KnownNat n) =>
    ElasticQueue n t ->
    (t -> Ix n -> Ivory eff ()) ->
    Ivory eff ()
popConcurrently ElasticQueue{..} handle = do
    isReady' <- deref isReady
    when isReady' do
        downConcurrently'
            consumerS
            do
                x <- deref consumerIx
                store consumerIx $ x + 1
                handle it x
                upConcurrently producerS
            do
                store isReady false

popConcurrently' ::
    (KnownNat n) =>
    ElasticQueue n t ->
    ( t ->
      Ix n ->
      Ivory eff ()
    ) ->
    Ivory eff () ->
    Ivory eff ()
popConcurrently' ElasticQueue{..} handleT handleF = do
    isReady' <- deref isReady
    ifte_
        isReady'
        do
            downConcurrently'
                consumerS
                do
                    x <- deref consumerIx
                    store consumerIx $ x + 1
                    handleT it x
                    upConcurrently producerS
                do
                    store isReady false
                    handleF
        handleF

remove ::
    (KnownNat n) =>
    ElasticQueue n t ->
    Ivory eff ()
remove ElasticQueue{..} = do
    isReady' <- deref isReady
    when isReady' do
        downConcurrently'
            consumerS
            do
                x <- deref consumerIx
                store consumerIx $ x + 1
                upConcurrently producerS
            do
                store isReady false

clearConcurrently ::
    forall n t eff.
    (KnownNat n) =>
    ElasticQueue n t ->
    Ivory eff ()
clearConcurrently ElasticQueue{..} = do
    store consumerIx 0
    store producerIx 0
    store (getSemaphore producerS) $ fromIntegral $ fromTypeNat (aNat :: NatType n)
    store (getSemaphore consumerS) 0
    store isReady false
