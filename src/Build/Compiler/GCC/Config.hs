module Build.Compiler.GCC.Config where


data GCC = GCC
    { defs    :: [String]
    , incs    :: [String]
    , libs    ::  String
    , cflags  :: [String]
    , ld      ::  String
    , ldflags :: [String]
    }
