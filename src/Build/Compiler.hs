{-# LANGUAGE MultiParamTypeClasses #-}

module Build.Compiler where
import           Core.Formula

class Compiler c p where
    mkCompiler :: Formula p -> c
