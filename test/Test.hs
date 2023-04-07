{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeApplications  #-}


module Test where

import           Data.Proxy
import           Ivory.Language        as L
import           Ivory.Language.Area
import           Ivory.Language.Struct
import           Ivory.Language.Syntax as S



instance IvoryStruct "foo" where
  structDef
    = StructDef
        (S.Struct "foo"
           [Typed (ivoryArea (Proxy :: Proxy ('Stored Uint32))) "f"])
f :: Label "foo" ('Stored Uint32)
f = Label "f"

instance IvoryStruct (Bar "bar") where
  structDef
    = StructDef
        (S.Struct "bar"
           [Typed (ivoryArea (Proxy :: Proxy ('L.Struct "foo"))) "b"])
b :: Label "bar" ('L.Struct "foo")
b = Label "b"

f' = [f .= ival 1]

x = area "x" . Just $ istruct f'
y = area "y" . Just $ istruct [b .= istruct f']


b' = addrOf y ~> b


type Bar t = "bar"
