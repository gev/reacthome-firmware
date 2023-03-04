{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Serialize where

import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Module



pack_uint8 :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n, Uint8] :-> ())
pack_uint8 = importProc "pack_uint8" "serialize.h"

unpack_uint8 :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n] :-> Uint8)
unpack_uint8 = importProc "unpack_uint8" "serialize.h"



pack_sint8 :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n, Sint8] :-> ())
pack_sint8 = importProc "pack_sint8" "serialize.h"

unpack_sint8 :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n] :-> Sint8)
unpack_sint8 = importProc "unpack_sint8" "serialize.h"



pack_uint16_be :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n, Uint16] :-> ())
pack_uint16_be = importProc "pack_uint16_be" "serialize.h"

unpack_uint16_be :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n] :-> Uint16)
unpack_uint16_be = importProc "unpack_uint16_be" "serialize.h"

pack_uint16_le :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n, Uint16] :-> ())
pack_uint16_le = importProc "pack_uint16_le" "serialize.h"

unpack_uint16_le :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n] :-> Uint16)
unpack_uint16_le = importProc "unpack_uint16_le" "serialize.h"



pack_sint16_be :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n, Sint16] :-> ())
pack_sint16_be = importProc "pack_sint16_be" "serialize.h"

unpack_sint16_be :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n] :-> Sint16)
unpack_sint16_be = importProc "unpack_sint16_be" "serialize.h"

pack_sint16_le :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n, Sint16] :-> ())
pack_sint16_le = importProc "pack_sint16_le" "serialize.h"

unpack_sint16_le :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n] :-> Sint16)
unpack_sint16_le = importProc "unpack_sint16_le" "serialize.h"



pack_uint32_be :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n, Uint32] :-> ())
pack_uint32_be = importProc "pack_uint32_be" "serialize.h"

unpack_uint32_be :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n] :-> Uint32)
unpack_uint32_be = importProc "unpack_uint32_be" "serialize.h"

pack_uint32_le :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n, Uint32] :-> ())
pack_uint32_le = importProc "pack_uint32_le" "serialize.h"

unpack_uint32_le :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n] :-> Uint32)
unpack_uint32_le = importProc "unpack_uint32_le" "serialize.h"



pack_sint32_be :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n, Sint32] :-> ())
pack_sint32_be = importProc "pack_sint32_be" "serialize.h"

unpack_sint32_be :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n] :-> Sint32)
unpack_sint32_be = importProc "unpack_sint32_be" "serialize.h"

pack_sint32_le :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n, Sint32] :-> ())
pack_sint32_le = importProc "pack_sint32_be" "serialize.h"

unpack_sint32_le :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Ix n] :-> Sint32)
unpack_sint32_le = importProc "unpack_sint32_le" "serialize.h"
