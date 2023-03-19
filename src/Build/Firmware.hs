{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Build.Firmware where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Domain
import           Core.Feature
import           Core.Formula
import           Core.Scheduler
import           Interface.MCU
import           Ivory.Compile.C.CmdlineFrontend
import           Ivory.Language
import           Ivory.Language.Module



cook :: Formula -> ModuleDef
cook (Formula model version mcu shouldInit transport features) = do

    inclModule
    incl  init
    incl  loop
    incl  main

    where (domain'   , domainContext'    ) = runWriter $ domain model version mcu' shouldInit transport' features'
          (transport', transportContext' ) = runReader (runWriterT transport) domain'
          (features' , featuresContext'  ) = unzip $ run <$> features
          (mcu'      , mcuContext'       ) = runWriter mcu

          run f = runReader (runWriterT f) domain'

          (Context inclModule inits tasks) = mcuContext'
                                          <> domainContext'
                                          <> transportContext'
                                          <> mconcat featuresContext'

          loop = mkLoop (systemClock mcu') tasks

          init :: Def ('[] :-> ())
          init = proc "init" $ body $ mapM_ call_ inits

          main :: Def ('[] :-> Sint32)
          main = proc "main" $ body $ do
            call_ init
            call_ loop
            ret 0



compile :: (ModuleDef, String) -> IO ()
compile (moduleDef, name) = runCompiler
    [package name moduleDef]
    []
    initialOpts
        { outDir = Just "./firmware"
        , constFold = True
        }
