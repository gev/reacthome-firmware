{-# LANGUAGE FlexibleInstances #-}

module Data.Serialize where

import GHC.TypeNats
import Ivory.Language
import Support.Serialize

class Serialize t where
    pack :: (KnownNat n) => Ref s (Array n (Stored Uint8)) -> Ix n -> t -> Ivory eff ()
    unpack :: (KnownNat n) => Ref s (Array n (Stored Uint8)) -> Ix n -> Ivory eff t

class SerializeBE t where
    packBE :: (KnownNat n) => Ref s (Array n (Stored Uint8)) -> Ix n -> t -> Ivory eff ()
    unpackBE :: (KnownNat n) => Ref s (Array n (Stored Uint8)) -> Ix n -> Ivory eff t

class SerializeLE t where
    packLE :: (KnownNat n) => Ref s (Array n (Stored Uint8)) -> Ix n -> t -> Ivory eff ()
    unpackLE :: (KnownNat n) => Ref s (Array n (Stored Uint8)) -> Ix n -> Ivory eff t

instance Serialize IBool where
    pack b i = call_ pack_bool (toCArray b) $ fromIx i
    unpack b i = call unpack_bool (toCArray b) $ fromIx i

instance SerializeBE IBool where
    packBE = pack
    unpackBE = unpack

instance SerializeLE IBool where
    packLE = pack
    unpackLE = unpack

instance Serialize Uint8 where
    pack b i = call_ pack_uint8 (toCArray b) $ fromIx i
    unpack b i = call unpack_uint8 (toCArray b) $ fromIx i

instance SerializeBE Uint8 where
    packBE = pack
    unpackBE = unpack

instance SerializeLE Uint8 where
    packLE = pack
    unpackLE = unpack

instance Serialize Sint8 where
    pack b i = call_ pack_sint8 (toCArray b) $ fromIx i
    unpack b i = call unpack_sint8 (toCArray b) $ fromIx i

instance SerializeBE Sint8 where
    packBE = pack
    unpackBE = unpack

instance SerializeLE Sint8 where
    packLE = pack
    unpackLE = unpack

instance SerializeBE Uint16 where
    packBE b i = call_ pack_uint16_be (toCArray b) $ fromIx i
    unpackBE b i = call unpack_uint16_be (toCArray b) $ fromIx i

instance SerializeLE Uint16 where
    packLE b i = call_ pack_uint16_le (toCArray b) $ fromIx i
    unpackLE b i = call unpack_uint16_le (toCArray b) $ fromIx i

instance SerializeBE Sint16 where
    packBE b i = call_ pack_sint16_be (toCArray b) $ fromIx i
    unpackBE b i = call unpack_sint16_be (toCArray b) $ fromIx i

instance SerializeLE Sint16 where
    packLE b i = call_ pack_sint16_le (toCArray b) $ fromIx i
    unpackLE b i = call unpack_sint16_le (toCArray b) $ fromIx i

instance SerializeBE Uint32 where
    packBE b i = call_ pack_uint32_be (toCArray b) $ fromIx i
    unpackBE b i = call unpack_uint32_be (toCArray b) $ fromIx i

instance SerializeLE Uint32 where
    packLE b i = call_ pack_uint32_le (toCArray b) $ fromIx i
    unpackLE b i = call unpack_uint32_le (toCArray b) $ fromIx i

instance SerializeBE Sint32 where
    packBE b i = call_ pack_sint32_be (toCArray b) $ fromIx i
    unpackBE b i = call unpack_sint32_be (toCArray b) $ fromIx i

instance SerializeLE Sint32 where
    packLE b i = call_ pack_sint32_le (toCArray b) $ fromIx i
    unpackLE b i = call unpack_sint32_le (toCArray b) $ fromIx i
