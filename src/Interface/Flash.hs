module Interface.Flash where

import           Ivory.Language
import           Ivory.Language.Module

newtype Addr = Addr {getAddr :: Uint32}

type Data = Uint32

class Flash m where
    address   :: m -> Addr -> Uint32
    write     :: m -> Addr -> Data -> Ivory eff ()
    read      :: m -> Addr -> Ivory eff Uint32
    erasePage :: m -> Ivory eff ()

addr = Addr
