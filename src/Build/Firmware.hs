{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}

module Build.Firmware where

import           Build.Compiler
import           Build.Compiler.GCC
import           Build.Compiler.GCC.GD32F3x0
import           Build.Shake
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Domain
import           Core.Feature
import           Core.Formula
import           Core.Scheduler
import           Interface.MCU
import           Ivory.Compile.C.CmdlineFrontend hiding (compile)
import           Ivory.Language
import           Ivory.Language.Module



cook :: Formula p -> MCU p -> Context -> ModuleDef
cook (Formula {..}) mcu' mcuContext' = do

    inclModule
    incl  init
    incl  loop
    incl  main

    where (domain'   , domainContext'    ) = runWriter $ domain model version mcu' shouldInit transport' features'
          (transport', transportContext' ) = runReader (runWriterT transport) domain'
          (features' , featuresContext'  ) = unzip $ run <$> features

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


build :: Compiler GCC p => [(Formula p, String)] -> Writer Context (MCU p) -> IO ()
build ms mcu' = do
    let (mcu, context) = runWriter mcu'
    let config :: GCC
        config = makeConfig mcu
    let run (f@(Formula {..}), name) = do
            compile (cook f mcu context, name)
    mapM_ run ms
    shake config $ snd <$> ms
