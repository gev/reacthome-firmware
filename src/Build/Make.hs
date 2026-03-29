module Build.Make where

import Build.Compiler
import Build.Formula
import Build.Formula.DFU
import Build.Shake
import Core.Formula
import Core.Formula.DFU

class Make f where
    make :: (Compiler c p, Shake c) => (Formula p -> Int -> Int -> c) -> f p -> IO ()

instance Make Formula where
    make = mkFormula

instance Make DFU where
    make = mkDFU
