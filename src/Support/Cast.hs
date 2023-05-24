{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Cast
    ( castArrayToUint32
    , castFloatToUint16
    , castFloatToUint8
    , inclCast
    ) where

import           Ivory.Language
import           Ivory.Language.Module

inclCast :: ModuleDef
inclCast = do
    incl cast_to_uint8
    incl cast_to_uint16
    incl cast_to_uint32


castArrayToUint32 :: Ref r (CArray (Stored Uint16)) -> Ivory eff Uint32
castArrayToUint32 = call cast_to_uint32

castFloatToUint16 :: IFloat -> Ivory eff Uint16
castFloatToUint16 = call cast_to_uint16

castFloatToUint8 :: IFloat -> Ivory eff Uint8
castFloatToUint8 = call cast_to_uint8

cast_to_uint32 :: Def ('[Ref r (CArray (Stored Uint16))] :-> Uint32)
cast_to_uint32 = importProc "cast_to_uint32" "cast.h"


cast_to_uint16 :: Def ('[IFloat] :-> Uint16)
cast_to_uint16 = importProc "cast_to_uint16" "cast.h"

cast_to_uint8 :: Def ('[IFloat] :-> Uint8)
cast_to_uint8 = importProc "cast_to_uint8" "cast.h"
