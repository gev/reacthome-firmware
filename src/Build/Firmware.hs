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
import           Data.Bifunctor
import           Data.List
import           Interface.MCU                   as I
import           Ivory.Compile.C.CmdlineFrontend
import           Ivory.Language
import           Ivory.Language.Module



cook :: Formula p -> ModuleDef
cook Formula{ ..} = do

    inclModule
    mapM_ incl multiBodyFunctions
    incl  init
    incl  loop
    incl  main

    where (domain'         , domainContext'        ) = runState (domain model version' mcu' shouldInit implementation') mempty
          (mcu'            , mcuContext'           ) = runState (platform mcu ) mempty
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

          version' = bimap fromIntegral fromIntegral version



generate :: ModuleDef -> String -> String -> IO ()
generate moduleDef path name = runCompiler
    [package name moduleDef]
    []
    initialOpts
        { outDir = Just $ "./firmware" <> "/" <> path
        , constFold = True
        }



build :: Shake c => c -> Formula p -> IO ()
build config f@Formula{..} = do
    let name' = name <> "-" <> I.model mcu <> I.modification mcu <> "-" <> major version <> "." <> minor version
    generate (cook f) name name'
    shake config $ name <> "/" <> name'

    where major = show . fst
          minor = show . snd
