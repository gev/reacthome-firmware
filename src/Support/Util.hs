{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Util
  ( castArrayToUint32
  , ptrOf
  , inclUtil
  ) where

import           Ivory.Language
import           Ivory.Language.Module

inclUtil :: ModuleM ()
inclUtil = do
  incl cast_to_uint32
  incl ptr_of


castArrayToUint32 :: Ref r (CArray (Stored Uint16)) -> Ivory eff Uint32
castArrayToUint32 = call cast_to_uint32

cast_to_uint32 :: Def ('[Ref r (CArray (Stored Uint16))] :-> Uint32)
cast_to_uint32 = importProc "cast_to_uint32" "util.h"


ptrOf :: Uint32 -> Ivory eff Uint32
ptrOf = call ptr_of

ptr_of :: Def ('[Uint32] :-> Uint32)
ptr_of = importProc "ptr_of" "util.h"
