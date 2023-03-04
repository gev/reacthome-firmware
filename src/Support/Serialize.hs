{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Serialize where

import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Module


pack_uint8 :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Uint16, Uint8] :-> ())
pack_uint8 = importProc "pack_uint8" "serialize.h"

unpack_uint8 :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Uint16] :-> Uint8)
unpack_uint8 = importProc "unpack_uint8" "serialize.h"


pack_sint8 :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Uint16, Sint8] :-> ())
pack_sint8 = importProc "pack_sint8" "serialize.h"

unpack_sint8 :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Uint16] :-> Sint8)
unpack_sint8 = importProc "unpack_sint8" "serialize.h"


pack_uint16 :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Uint16, Uint16] :-> ())
pack_uint16 = importProc "pack_uint16" "serialize.h"

unpack_uint16 :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Uint16] :-> Uint16)
unpack_uint16 = importProc "unpack_uint16" "serialize.h"


pack_sint16 :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Uint16, Sint16] :-> ())
pack_sint16 = importProc "pack_sint16" "serialize.h"

unpack_sint16 :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Uint16] :-> Sint16)
unpack_sint16 = importProc "unpack_sint16" "serialize.h"


pack_uint32 :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Uint16, Uint32] :-> ())
pack_uint32 = importProc "pack_uint32" "serialize.h"

unpack_uint32 :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Uint16] :-> Uint32)
unpack_uint32 = importProc "unpack_uint32" "serialize.h"


pack_sint32 :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Uint16, Sint32] :-> ())
pack_sint32 = importProc "pack_sint32" "serialize.h"

unpack_sint32 :: KnownNat n => Def ('[Ref s (Array n (Stored Uint8)), Uint16] :-> Sint32)
unpack_sint32 = importProc "unpack_sint32" "serialize.h"
