{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language

printf :: Def ('[IString, Uint32] :-> ())
printf = importProc "printf" "stdio.h"

put :: Uint32 -> Ivory eff ()
put = call_ printf "%u\n"

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
  a <- call fibLoop 10
  put a
  ret 0

fibTutorialModule :: Module
fibTutorialModule = package "fib_tutorial" $ do
  incl fibLoop
  incl main'
  incl printf

main :: IO ()
main =
  runCompiler
    [fibTutorialModule]
    []
    initialOpts
      { outDir = Just "./build",
        constFold = True
      }
