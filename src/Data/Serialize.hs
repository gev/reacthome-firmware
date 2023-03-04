{-# LANGUAGE DataKinds #-}

module Data.Serialize where

import           GHC.TypeNats
import           Ivory.Language
import           Support.Serialize





class Serialize t where
    pack   :: KnownNat n => Ref s (Array n (Stored Uint8)) -> Ix n -> t -> Ivory eff ()
    unpack :: KnownNat n => Ref s (Array n (Stored Uint8)) -> Ix n -> Ivory eff t

class SerializeBE t where
    packBE   :: KnownNat n => Ref s (Array n (Stored Uint8)) -> Ix n -> t -> Ivory eff ()
    unpackBE :: KnownNat n => Ref s (Array n (Stored Uint8)) -> Ix n -> Ivory eff t

class SerializeLE t where
    packLE   :: KnownNat n => Ref s (Array n (Stored Uint8)) -> Ix n -> t -> Ivory eff ()
    unpackLE :: KnownNat n => Ref s (Array n (Stored Uint8)) -> Ix n -> Ivory eff t



instance Serialize Uint8 where
    pack   b i = call_ pack_uint8   b $ toIx i
    unpack b i = call  unpack_uint8 b $ toIx i

instance SerializeBE Uint8 where
    packBE   = pack
    unpackBE = unpack

instance SerializeLE Uint8 where
    packLE   = pack
    unpackLE = unpack




instance Serialize Sint8 where
    pack   b i = call_ pack_sint8   b $ toIx i
    unpack b i = call  unpack_sint8 b $ toIx i

instance SerializeBE Sint8 where
    packBE   = pack
    unpackBE = unpack

instance SerializeLE Sint8 where
    packLE   = pack
    unpackLE = unpack
