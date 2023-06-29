{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Interface.Flash where

import           Ivory.Language
import           Ivory.Language.Module

newtype Addr = Addr {getAddr :: Uint32}
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar, Num)

type Data = Uint32

class Flash f where
    address   :: f -> Addr -> Uint32
    write     :: f -> Addr -> Data -> Ivory eff ()
    read      :: f -> Addr -> Ivory eff Uint32
    erasePage :: f -> Ivory eff ()

addr = Addr
