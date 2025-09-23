module Support.Serialize where

import GHC.TypeNats
import Ivory.Language
import Ivory.Language.Module

inclSerialize :: ModuleDef
inclSerialize = do
    incl pack_bool
    incl unpack_bool
    incl pack_uint8
    incl unpack_uint8
    incl pack_sint8
    incl unpack_sint8
    incl pack_uint16_be
    incl unpack_uint16_be
    incl pack_uint16_le
    incl unpack_uint16_le
    incl pack_sint16_be
    incl unpack_sint16_be
    incl pack_sint16_le
    incl unpack_sint16_le
    incl pack_uint32_be
    incl unpack_uint32_be
    incl pack_uint32_le
    incl unpack_uint32_le
    incl pack_sint32_be
    incl unpack_sint32_be
    incl pack_sint32_le
    incl unpack_sint32_le

pack_bool :: Def ('[Ref s (CArray (Stored Uint8)), Sint32, IBool] :-> ())
pack_bool = importProc "pack_bool" "serialize.h"

unpack_bool :: Def ('[Ref s (CArray (Stored Uint8)), Sint32] :-> IBool)
unpack_bool = importProc "unpack_bool" "serialize.h"

pack_uint8 :: Def ('[Ref s (CArray (Stored Uint8)), Sint32, Uint8] :-> ())
pack_uint8 = importProc "pack_uint8" "serialize.h"

unpack_uint8 :: Def ('[Ref s (CArray (Stored Uint8)), Sint32] :-> Uint8)
unpack_uint8 = importProc "unpack_uint8" "serialize.h"

pack_sint8 :: Def ('[Ref s (CArray (Stored Uint8)), Sint32, Sint8] :-> ())
pack_sint8 = importProc "pack_sint8" "serialize.h"

unpack_sint8 :: Def ('[Ref s (CArray (Stored Uint8)), Sint32] :-> Sint8)
unpack_sint8 = importProc "unpack_sint8" "serialize.h"

pack_uint16_be :: Def ('[Ref s (CArray (Stored Uint8)), Sint32, Uint16] :-> ())
pack_uint16_be = importProc "pack_uint16_be" "serialize.h"

unpack_uint16_be :: Def ('[Ref s (CArray (Stored Uint8)), Sint32] :-> Uint16)
unpack_uint16_be = importProc "unpack_uint16_be" "serialize.h"

pack_uint16_le :: Def ('[Ref s (CArray (Stored Uint8)), Sint32, Uint16] :-> ())
pack_uint16_le = importProc "pack_uint16_le" "serialize.h"

unpack_uint16_le :: Def ('[Ref s (CArray (Stored Uint8)), Sint32] :-> Uint16)
unpack_uint16_le = importProc "unpack_uint16_le" "serialize.h"

pack_sint16_be :: Def ('[Ref s (CArray (Stored Uint8)), Sint32, Sint16] :-> ())
pack_sint16_be = importProc "pack_sint16_be" "serialize.h"

unpack_sint16_be :: Def ('[Ref s (CArray (Stored Uint8)), Sint32] :-> Sint16)
unpack_sint16_be = importProc "unpack_sint16_be" "serialize.h"

pack_sint16_le :: Def ('[Ref s (CArray (Stored Uint8)), Sint32, Sint16] :-> ())
pack_sint16_le = importProc "pack_sint16_le" "serialize.h"

unpack_sint16_le :: Def ('[Ref s (CArray (Stored Uint8)), Sint32] :-> Sint16)
unpack_sint16_le = importProc "unpack_sint16_le" "serialize.h"

pack_uint32_be :: Def ('[Ref s (CArray (Stored Uint8)), Sint32, Uint32] :-> ())
pack_uint32_be = importProc "pack_uint32_be" "serialize.h"

unpack_uint32_be :: Def ('[Ref s (CArray (Stored Uint8)), Sint32] :-> Uint32)
unpack_uint32_be = importProc "unpack_uint32_be" "serialize.h"

pack_uint32_le :: Def ('[Ref s (CArray (Stored Uint8)), Sint32, Uint32] :-> ())
pack_uint32_le = importProc "pack_uint32_le" "serialize.h"

unpack_uint32_le :: Def ('[Ref s (CArray (Stored Uint8)), Sint32] :-> Uint32)
unpack_uint32_le = importProc "unpack_uint32_le" "serialize.h"

pack_sint32_be :: Def ('[Ref s (CArray (Stored Uint8)), Sint32, Sint32] :-> ())
pack_sint32_be = importProc "pack_sint32_be" "serialize.h"

unpack_sint32_be :: Def ('[Ref s (CArray (Stored Uint8)), Sint32] :-> Sint32)
unpack_sint32_be = importProc "unpack_sint32_be" "serialize.h"

pack_sint32_le :: Def ('[Ref s (CArray (Stored Uint8)), Sint32, Sint32] :-> ())
pack_sint32_le = importProc "pack_sint32_be" "serialize.h"

unpack_sint32_le :: Def ('[Ref s (CArray (Stored Uint8)), Sint32] :-> Sint32)
unpack_sint32_le = importProc "unpack_sint32_le" "serialize.h"
