{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}

module Build.Firmware where

import           Build.Shake
import           Control.Monad.Reader
import           Control.Monad.State
import           Core.Context
import           Core.Domain
import           Core.Formula
import           Core.Scheduler
import           Data.List
import           Interface.MCU
import           Ivory.Compile.C.CmdlineFrontend
import           Ivory.Language
import           Ivory.Language.Module



cook :: State Context (MCU p) -> Formula p -> ModuleDef
cook mcu Formula{..} = do


    inclModule
    mapM_ incl multiBodyFunctions
    incl  init
    incl  loop
    incl  main


    where (domain'         , domainContext'        ) = runState (domain model version mcu' shouldInit implementation') mempty
          (mcu'            , mcuContext'           ) = runState mcu mempty
          (implementation' , implementationContext') = runReader (runStateT implementation mempty) domain'

          (Context inclModule inits tasks syncs bodies) = mcuContext'
                                                       <> domainContext'
                                                       <> implementationContext'

          bodyNames = nub $ fst <$> bodies

          multiBodyFunctions = mkMultiBodyFunction <$> bodyNames

          mkMultiBodyFunction :: String -> Def ('[] :-> ())
          mkMultiBodyFunction name = proc name $ body $ mapM_ snd $ filter (\(id, _) -> id == name) bodies

          loop = mkLoop (systemClock mcu') tasks

          init :: Def ('[] :-> ())
          init = proc "init" $ body $ mapM_ call_ inits

          main :: Def ('[] :-> Sint32)
          main = proc "main" $ body $ do
            call_ init
            call_ loop
            ret 0



generate :: ModuleDef -> String -> IO ()
generate moduleDef name = runCompiler
    [package name moduleDef]
    []
    initialOpts
        { outDir = Just "./firmware"
        , constFold = True
        }



build :: Shake c
      => c -> MCUmod p -> [Formula p] -> IO ()
build config MCUmod{..} formulas =
    shake config =<< mapM run formulas
    where
        run f@Formula{..} = do
            generate (cook mcu f) name
            pure name
