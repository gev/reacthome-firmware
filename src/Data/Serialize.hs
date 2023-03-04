{-# LANGUAGE DataKinds #-}

module Data.Serialize where

import           GHC.TypeNats
import           Ivory.Language





class Serialize t where
    pack   :: KnownNat n => Ref s (Array n (Stored Uint8)) -> t -> Ix n -> Ivory eff ()
    unpack :: KnownNat n => Ref s (Array n (Stored Uint8)) -> Ix n -> Ivory eff t

class SerializeBE t where
    packBE   :: KnownNat n => Ref s (Array n (Stored Uint8)) -> t -> Ix n -> Ivory eff ()
    unpackBE :: KnownNat n => Ref s (Array n (Stored Uint8)) -> Ix n -> Ivory eff t

class SerializeLE t where
    packLE   :: KnownNat n => Ref s (Array n (Stored Uint8)) -> t -> Ix n -> Ivory eff ()
    unpackLE :: KnownNat n => Ref s (Array n (Stored Uint8)) -> Ix n -> Ivory eff t



instance Serialize Uint8 where
    pack b v i = store (b ! i) v
    unpack b i = deref (b ! i)

instance SerializeBE Uint8 where
    packBE   = pack
    unpackBE = unpack

instance SerializeLE Uint8 where
    packLE   = pack
    unpackLE = unpack




-- instance Serialize Sint8 where
--     pack b v i = store (b ! i) v
--     unpack b i = deref (b ! i)

-- instance SerializeBE Sint8 where
--     packBE   = pack
--     unpackBE = unpack

-- instance SerializeLE Sint8 where
--     packLE   = pack
--     unpackLE = unpack
