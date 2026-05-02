module Interface.Flash where

import Ivory.Language

newtype Addr = Addr {getAddr :: Uint32}
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar, Num)

class Flash f where
    address :: f -> Addr -> Uint32
    write :: f -> Addr -> Uint32 -> Ivory eff ()
    read :: f -> Addr -> Ivory eff Uint32
    erasePage :: f -> Addr -> Ivory eff ()

addr = Addr
