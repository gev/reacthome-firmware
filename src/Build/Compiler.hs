{-# LANGUAGE MultiParamTypeClasses #-}

module Build.Compiler where

import           Interface.MCU

class Compiler c p where
    mkCompiler :: MCUmod p -> c
