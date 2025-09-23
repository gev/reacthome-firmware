module Build.Compiler.GCC.Config where

data GCC = GCC
    { path :: String
    , defs :: [String]
    , incs :: [String]
    , libs :: [String]
    , cflags :: [String]
    , ld :: String
    , ldflags :: [String]
    }
