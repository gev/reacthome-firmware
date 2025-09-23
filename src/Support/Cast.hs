{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Support.Cast (
    castArrayUint8ToUint32,
    castArrayUint16ToUint32,
    castArrayUint32ToUint32,
    castConstArrayUint8ToUint32,
    castConstArrayUint16ToUint32,
    castFloatToUint16,
    castFloatToUint8,
    derefUint32,
    inclCast,
) where

import Ivory.Language
import Ivory.Language.Module

inclCast :: ModuleDef
inclCast = do
    incl cast_to_uint8
    incl cast_to_uint16
    incl cast_array_uint8_to_uint32
    incl cast_array_uint16_to_uint32
    incl cast_array_uint32_to_uint32
    incl deref_uint32

castArrayUint8ToUint32 :: Ref s (CArray (Stored Uint8)) -> Ivory eff Uint32
castArrayUint8ToUint32 = call cast_array_uint8_to_uint32

cast_array_uint8_to_uint32 :: Def ('[Ref s (CArray (Stored Uint8))] :-> Uint32)
cast_array_uint8_to_uint32 = importProc "cast_to_uint32" "cast.h"

castArrayUint16ToUint32 :: Ref s (CArray (Stored Uint16)) -> Ivory eff Uint32
castArrayUint16ToUint32 = call cast_array_uint16_to_uint32

cast_array_uint16_to_uint32 :: Def ('[Ref s (CArray (Stored Uint16))] :-> Uint32)
cast_array_uint16_to_uint32 = importProc "cast_to_uint32" "cast.h"

castArrayUint32ToUint32 :: Ref s (CArray (Stored Uint32)) -> Ivory eff Uint32
castArrayUint32ToUint32 = call cast_array_uint32_to_uint32

cast_array_uint32_to_uint32 :: Def ('[Ref s (CArray (Stored Uint32))] :-> Uint32)
cast_array_uint32_to_uint32 = importProc "cast_to_uint32" "cast.h"

castConstArrayUint8ToUint32 :: ConstRef s (CArray (Stored Uint8)) -> Ivory eff Uint32
castConstArrayUint8ToUint32 = call cast_const_array_uint8_to_uint32

cast_const_array_uint8_to_uint32 :: Def ('[ConstRef s (CArray (Stored Uint8))] :-> Uint32)
cast_const_array_uint8_to_uint32 = importProc "cast_to_uint32" "cast.h"

castConstArrayUint16ToUint32 :: ConstRef s (CArray (Stored Uint16)) -> Ivory eff Uint32
castConstArrayUint16ToUint32 = call cast_const_array_uint16_to_uint32

cast_const_array_uint16_to_uint32 :: Def ('[ConstRef s (CArray (Stored Uint16))] :-> Uint32)
cast_const_array_uint16_to_uint32 = importProc "cast_to_uint32" "cast.h"

castFloatToUint16 :: IFloat -> Ivory eff Uint16
castFloatToUint16 = call cast_to_uint16

castFloatToUint8 :: IFloat -> Ivory eff Uint8
castFloatToUint8 = call cast_to_uint8

cast_to_uint16 :: Def ('[IFloat] :-> Uint16)
cast_to_uint16 = importProc "cast_to_uint16" "cast.h"

cast_to_uint8 :: Def ('[IFloat] :-> Uint8)
cast_to_uint8 = importProc "cast_to_uint8" "cast.h"

derefUint32 :: Uint32 -> Ivory eff Uint32
derefUint32 = call deref_uint32

deref_uint32 :: Def ('[Uint32] :-> Uint32)
deref_uint32 = importProc "deref_uint32" "cast.h"
