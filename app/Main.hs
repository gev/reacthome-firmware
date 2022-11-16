{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language
import Support.Device.GD32F3x0.RCU
import Ivory.Language.Syntax.Concrete.ParseAST
import Ivory.Language.Proc

main' :: Def ('[] :-> Sint32)
main' = proc "main" $ body $ do
  enablePeriphClock RCU_GPIOA
  ret 0

blinkModule :: Module
blinkModule = package "blink" $ do
  depend rcuModule
  incl main'

main :: IO ()
main =
  runCompiler
    [blinkModule, rcuModule]
    []
    initialOpts
      { outDir = Just "./build",
        constFold = True
      }
