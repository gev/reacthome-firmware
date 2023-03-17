{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Concurrent.Queue where

import           Control.Monad.Writer      (WriterT)
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


queue :: forall m n. (Monad m, KnownNat n)
      => String -> WriterT Context m (Queue n)
queue id = do
    let name        = id    <>  "_queue"
    let producerId  = name  <>  "_producer"
    let consumerId  = name  <>  "_consumer"
    producerIx     <- index     producerId
    consumerIx     <- index     consumerId
    producerS      <- semaphore producerId $ fromInteger $ fromTypeNat (aNat :: NatType n)
    consumerS      <- semaphore consumerId 0
    pure Queue { producerIx, consumerIx, producerS, consumerS }


push :: Queue n -> (Uint16 -> Ivory eff ()) -> Ivory eff ()
push (Queue {..}) handle =
    down producerS $ do
        x <- deref $ addrOf producerIx
        handle x
        store (addrOf producerIx) $ x + 1
        up consumerS


pop :: Queue n -> (Uint16 -> Ivory eff ()) -> Ivory eff ()
pop (Queue {..}) handle =
    down consumerS $ do
        x <- deref $ addrOf consumerIx
        handle x
        store (addrOf consumerIx) $ x + 1
        up producerS


peek :: Queue n -> (Uint16 -> Ivory eff ()) -> Ivory eff ()
peek (Queue {..}) handle = do
    check consumerS $ do
        x <- deref $ addrOf consumerIx
        handle x


remove :: Queue n -> Ivory eff ()
remove (Queue {..}) =
    down consumerS $ do
        x <- deref $ addrOf consumerIx
        store (addrOf consumerIx) $ x + 1
        up producerS


size :: Queue n -> Ivory eff Uint16
size (Queue {..}) =
    (-) <$> deref (addrOf producerIx) <*> deref (addrOf consumerIx)
