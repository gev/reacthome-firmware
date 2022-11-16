{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Ivory.Compile.C.CmdlineFrontend
import           Ivory.Language
import           Ivory.Language.Proc
import           Ivory.Language.Syntax.Concrete.ParseAST
import           Support.Device.GD32F3x0

main' :: Def ('[] :-> Sint32)
main' = proc "main" $ body $ do
  enablePeriphClock RCU_GPIOA
  ret 0

blinkModule :: Module
blinkModule = package "blink" $ do
  depend rcuModule
  incl main'

compileBlink :: IO ()
compileBlink =
  runCompiler
    [blinkModule, rcuModule]
    []
    initialOpts
      { outDir = Just "./build"
      , constFold = True
      }

main :: IO ()
main = compileBlink
