{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Firmware
  ( Firmware.compile
  , cook
  ) where

import           Data.Foldable
import           Feature
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

cook :: Features -> ModuleM ()
cook fs = do
  let ps = prepare <$> fs
  sequenceA_ $ dependecies =<< ps
  inclT initialize ps
  inclT step ps
  let i = init' ps
  let l = loop' ps
  incl $ init' ps
  incl $ loop' ps
  incl $ main' i l

init' :: [Pack] -> Def ('[] ':-> ())
init' ps = proc "init" $ body $ callT_ initialize ps

loop' :: [Pack] -> Def ('[] ':-> ())
loop' ps = proc "loop" $ body $ forever $ callT_ step ps

main' ::Def ('[] ':-> ()) -> Def ('[] ':-> ()) -> Def ('[] ':-> Sint32)
main' i l = proc "main" $ body $ call_ i >> call_ l >> ret 0

inclT :: (Pack -> Def ('[] ':-> ())) -> [Pack] -> ModuleM ()
inclT f  = traverse_ (incl . f)

callT_ :: (Pack -> Def ('[] ':-> ())) -> [Pack] -> Ivory eff ()
callT_ f  = traverse_ (call_ . f)
