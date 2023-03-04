{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Serialize where

import           Ivory.Language
import           Ivory.Language.Module


pack_uint8 :: Def ('[Ref s (Array n (Store Uint8)), Uint16, Uint8] :-> ())
pack_uint8 = importProc "pack_uint8" "serialize.h"

unpack_uint8 :: Def ('[Ref s (Array n (Store Uint8)), Uint16] :-> Uint8)
unpack_uint8 = importProc "unpack_uint8" "serialize.h"


pack_int8 :: Def ('[Ref s (Array n (Store Uint8)), Uint16, Int8] :-> ())
pack_int8 = importProc "pack_int8" "serialize.h"

unpack_int8 :: Def ('[Ref s (Array n (Store Uint8)), Uint16] :-> Int8)
unpack_int8 = importProc "unpack_int8" "serialize.h"


pack_uint16 :: Def ('[Ref s (Array n (Store Uint8)), Uint16, Uint16] :-> ())
pack_uint16 = importProc "pack_uint16" "serialize.h"

unpack_uint16 :: Def ('[Ref s (Array n (Store Uint8)), Uint16] :-> Uint16)
unpack_uint16 = importProc "unpack_uint16" "serialize.h"


pack_int16 :: Def ('[Ref s (Array n (Store Uint8)), Uint16, Int16] :-> ())
pack_int16 = importProc "pack_int16" "serialize.h"

unpack_int16 :: Def ('[Ref s (Array n (Store Uint8)), Uint16] :-> Int16)
unpack_int16 = importProc "unpack_int16" "serialize.h"


pack_uint32 :: Def ('[Ref s (Array n (Store Uint8)), Uint16, Uint32] :-> ())
pack_uint32 = importProc "pack_uint32" "serialize.h"

unpack_uint32 :: Def ('[Ref s (Array n (Store Uint8)), Uint16] :-> Uint32)
unpack_uint32 = importProc "unpack_uint32" "serialize.h"


pack_int32 :: Def ('[Ref s (Array n (Store Uint8)), Uint16, Int32] :-> ())
pack_int32 = importProc "pack_int32" "serialize.h"

unpack_int32 :: Def ('[Ref s (Array n (Store Uint8)), Uint16] :-> Int32)
unpack_int32 = importProc "unpack_int32" "serialize.h"