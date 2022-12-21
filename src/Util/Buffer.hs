{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Util.Buffer where

import           GHC.TypeLits
import           Include
import           Ivory.Language
import           Ivory.Language.Array
import           Ivory.Stdlib
import           Util.Semaphore


data Buffer n t = Buffer
  { array      :: MemArea (Array n (Stored t))
  , producerIx :: MemArea (Stored (Ix n))
  , producerS  :: Semaphore
  , consumerIx :: MemArea (Stored (Ix n))
  , consumerS  :: Semaphore
  }


index :: KnownNat n => String -> MemArea (Stored (Ix n))
index id = area ("index_" <> id) $ Just $ ival (toIx (0 :: Sint32))


buffer :: (KnownNat n, IvoryType t, IvoryZeroVal t)
       => String -> Buffer n t
buffer id =
  let name       = "buffer_"   <> id
      producerId = "producer_" <> name
      consumerId = "consumer_" <> name
      a = area name Nothing
  in Buffer { array       = a
            , producerIx  = index     producerId
            , producerS   = semaphore producerId $ arrayLen $ addrOf a
            , consumerIx  = index     consumerId
            , consumerS   = semaphore consumerId 0
            }


write :: (IvoryStore t, KnownNat n)
      => Buffer n t -> t -> Ivory eff ()
write (Buffer {array, producerIx, producerS, consumerS}) v =
  down producerS $ do
    let a = addrOf array
    let pIx = addrOf producerIx
    ix <- deref pIx
    store (a ! ix) v
    store pIx $ ix + 1
    up consumerS


read :: (IvoryStore t, KnownNat n)
     => Buffer n t -> (t -> Ivory eff ()) -> Ivory eff ()
read (Buffer {array, consumerIx, consumerS, producerS}) run =
  down consumerS $ do
    let a = addrOf array
    let cIx = addrOf consumerIx
    ix <- deref cIx
    v <- deref (a ! ix)
    store cIx $ ix + 1
    up producerS
    run v


instance (KnownNat n, IvoryType t) => Include (Buffer n t) where
  include (Buffer array producerIx producerS consumerIx consumerS) = do
    defMemArea  array
    defMemArea  producerIx
    defMemArea  consumerIx
    include     producerS
    include     consumerS
