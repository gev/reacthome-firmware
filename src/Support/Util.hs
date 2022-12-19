{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Support.Util
  ( castBuffRefToUint32
  , inclUtil
  ) where

import           Ivory.Language

inclUtil = [incl cast_to_uint32]

castBuffRefToUint32 :: Ref r ('Array 512 ('Stored Uint16)) -> Ivory eff Uint32
castBuffRefToUint32 = call cast_to_uint32

cast_to_uint32 :: Def ('[Ref r ('Array 512 ('Stored Uint16))] :-> Uint32)
cast_to_uint32 = importProc "cast_to_uint32" "util.h"
