{-# LANGUAGE MultiParamTypeClasses #-}

module Build.Compiler where

import           Interface.MCU

class Compiler c p where
    makeConfig :: MCU p -> c
