{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Support.Util
  ( castArrayToUint32
  , inclUtil
  ) where

import           Ivory.Language

inclUtil = [incl cast_to_uint32]

castArrayToUint32 :: Ref r (CArray (Stored Uint16)) -> Ivory eff Uint32
castArrayToUint32 = call cast_to_uint32

cast_to_uint32 :: Def ('[Ref r (CArray (Stored Uint16))] :-> Uint32)
cast_to_uint32 = importProc "cast_to_uint32" "util.h"
