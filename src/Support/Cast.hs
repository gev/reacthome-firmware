{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Cast
    ( castArrayToUint32
    , inclCast
    ) where

import           Ivory.Language
import           Ivory.Language.Module

inclCast :: ModuleM ()
inclCast = incl cast_to_uint32


castArrayToUint32 :: Ref r (CArray (Stored Uint16)) -> Ivory eff Uint32
castArrayToUint32 = call cast_to_uint32

cast_to_uint32 :: Def ('[Ref r (CArray (Stored Uint16))] :-> Uint32)
cast_to_uint32 = importProc "cast_to_uint32" "cast.h"
