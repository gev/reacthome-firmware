{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}

module Build.Firmware where

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



cook :: Formula p -> MCUmod p -> ModuleDef
cook (Formula {..}) (MCUmod {mcu}) = do

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



compile :: ModuleDef -> String -> IO ()
compile moduleDef name = runCompiler
    [package name moduleDef]
    []
    initialOpts
        { outDir = Just "./firmware"
        , constFold = True
        }



build :: Shake c => c -> MCUmod p -> [(Formula p, String)] -> IO ()
build config mcu ms = do
    mapM_ run ms
    shake config $ snd <$> ms
    where
        run (f@(Formula {..}), name) = do
            compile (cook f mcu) name
