{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language

fibLoop :: Def ('[Ix 1000] :-> Uint32)
fibLoop = proc "fib_loop" $ \n -> body $ do
  a <- local (ival 0)
  b <- local (ival 1)
  n `times` \_ -> do
    a' <- deref a
    b' <- deref b
    store a b'
    store b (a' + b')
  result <- deref a
  ret result

main' :: Def ('[] :-> Sint32)
main' = proc "main" $ body $ do
  call_ fibLoop 10
  ret 0

fibTutorialModule :: Module
fibTutorialModule = package "fib_tutorial" $ do
  incl fibLoop
  incl main'

main :: IO ()
main =
  runCompiler
    [fibTutorialModule]
    []
    initialOpts
      { outDir = Just "./build",
        constFold = True
      }
