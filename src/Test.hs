{-# LANGUAGE DataKinds #-}

module Test where
import           Data.Proxy
import           Ivory.Language        as L
import           Ivory.Language.Area
import           Ivory.Language.Struct
import           Ivory.Language.Syntax as S


instance IvoryStruct "Foo" where
  structDef
    = StructDef
        (S.Struct "Foo"
           [Typed (ivoryArea (Proxy :: Proxy (L.Stored Uint32))) "f"])

f :: Label "Foo" (L.Stored Uint32)
f = Label "f"


instance IvoryStruct "bar" where
  structDef
    = StructDef
        (S.Struct "bar"
           [Typed (ivoryArea (Proxy :: Proxy ('L.Struct "Foo"))) "b"])

b :: Label "bar" ('L.Struct "Foo")
b = Label "b"

x = area "x" . Just $ istruct [f .= ival 1]
y = area "y" . Just $ istruct [b .= istruct [f .= ival 1]]


b' = addrOf y ~> b


type Foo' = "Foo"
