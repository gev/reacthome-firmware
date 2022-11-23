module Firmware where

import           Ivory.Compile.C.CmdlineFrontend
import           Ivory.Language

compile :: ModuleDef -> String -> IO ()
compile m n = runCompiler
  [package n m]
  []
  initialOpts
    { outDir = Just "./firmware"
    , constFold = True
    }
