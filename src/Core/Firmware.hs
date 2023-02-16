module Core.Firmware where

import           Core.Feature
import           Data.Foldable
import           Ivory.Compile.C.CmdlineFrontend
import           Ivory.Language
import           Ivory.Language.Module


compile :: (ModuleDef, String) -> IO ()
compile (m, n) = runCompiler
    [package n m]
    []
    initialOpts
        { outDir = Just "./firmware"
        , constFold = True
        }