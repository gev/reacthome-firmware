{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Concurrent.ElasticQueue where

import           Control.Monad.State       (MonadState)
import           Core.Context
import           Data.Concurrent.Semaphore
import           Data.Index
import           Data.Value
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Array
import           Ivory.Language.Proxy
import           Ivory.Stdlib


{--
    TODO: Make polymorph?
--}
data ElasticQueue (n :: Nat) = ElasticQueue
    { producerIx :: Index Uint16
    , consumerIx :: Index Uint16
    , producerS  :: Semaphore Uint16
    , consumerS  :: Semaphore Uint16
    , isReady    :: Value IBool
    , half       :: Uint16
    }


elastic :: forall m n. (MonadState Context m, KnownNat n)
        => String -> m (ElasticQueue n)
elastic id = do
    let size        = fromIntegral $ fromTypeNat (aNat :: NatType n)
    let half        = size `iDiv` 2
    let name        = id    <>  "_queue"
    let producerId  = name  <>  "_producer"
    let consumerId  = name  <>  "_consumer"
    producerIx     <- index     producerId
    consumerIx     <- index     consumerId
    producerS      <- semaphore producerId $ safeCast size
    consumerS      <- semaphore consumerId 0
    isReady        <- value (name <> "_is_ready") false
    pure ElasticQueue { producerIx, consumerIx, producerS, consumerS, isReady, half }


push :: ElasticQueue n -> (Uint16 -> Ivory eff ()) -> Ivory eff ()
push q@ElasticQueue{..} handle = do
    down producerS $ do
        x <- deref producerIx
        store producerIx $ x + 1
        handle x
        up consumerS
        s <- size q
        when (s >=? half) $ store isReady true


push' :: ElasticQueue n -> (Uint16 -> Ivory eff ()) -> Ivory eff () -> Ivory eff ()
push' q@ElasticQueue{..} handleT handleF = do
    flip (down' producerS) handleF $ do
        x <- deref producerIx
        store producerIx $ x + 1
        handleT x
        up consumerS
        s <- size q
        when (s >=? half) $ store isReady true


pop :: ElasticQueue n -> (Uint16 -> Ivory eff ()) -> Ivory eff ()
pop ElasticQueue{..} handle = do
    isReady' <- deref isReady
    when isReady' $ do
        flip (down' consumerS) (store isReady false) $ do
            x <- deref consumerIx
            store consumerIx $ x + 1
            handle x
            up producerS


pop' :: ElasticQueue n -> (Uint16 -> Ivory eff ()) -> Ivory eff () -> Ivory eff ()
pop' ElasticQueue{..} handleT handleF = do
    isReady' <- deref isReady
    flip (ifte_ isReady') handleF $ do
        flip (down' consumerS) (store isReady false >> handleF) $ do
            x <- deref consumerIx
            store consumerIx $ x + 1
            handleT x
            up producerS


peek :: ElasticQueue n -> (Uint16 -> Ivory eff ()) -> Ivory eff ()
peek ElasticQueue{..} handle = do
    isReady' <- deref isReady
    when isReady' $ do
        flip (check' consumerS) (store isReady false) $ do
            x <- deref consumerIx
            handle x


peek' :: ElasticQueue n -> (Uint16 -> Ivory eff ()) -> Ivory eff () -> Ivory eff ()
peek' ElasticQueue{..} handleT handleF = do
    isReady' <- deref isReady
    flip (ifte_ isReady') handleF $ do
        flip (check' consumerS) (store isReady false >> handleF) $ do
            x <- deref consumerIx
            handleT x


size :: ElasticQueue n -> Ivory eff Uint16
size ElasticQueue{..} =
    deref $ getSemaphore consumerS


remove :: ElasticQueue n -> Ivory eff ()
remove ElasticQueue{..} = do
    isReady' <- deref isReady
    when isReady' $ do
        flip (down' consumerS) (store isReady false) $ do
            x <- deref consumerIx
            store consumerIx $ x + 1
            up producerS



clear :: forall n eff. KnownNat n => ElasticQueue n -> Ivory eff ()
clear ElasticQueue{..} = do
    store consumerIx 0
    store producerIx 0
    store (getSemaphore producerS) $ fromIntegral $ fromTypeNat (aNat :: NatType n)
    store (getSemaphore consumerS) 0
    store isReady false
