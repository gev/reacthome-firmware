module Build.Formula where

import Build.Compiler
import Build.Shake
import Control.Monad (void)
import Control.Monad.Reader
import Control.Monad.State
import Core.Context
import Core.Domain (domain)
import Core.Formula (Formula (Formula, implementation, meta))
import Core.Meta (board, mcu, mkName, model, shouldInit, version)
import Core.Scheduler
import Data.Bifunctor
import Data.List (nub)
import Development.Shake.FilePath
import Interface.MCU (MCU (sizeFlash, startFlash), Platform (..), platform)
import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language

cook :: Formula p -> ModuleDef
cook Formula{..} = do
    inclModule
    mapM_ incl multiBodyFunctions
    incl initialize
    incl loop
    incl main
  where
    (domain', domainContext') = runState (domain (fromIntegral meta.model) version' mcu' (fromIntegral meta.board) meta.shouldInit implementation') mempty
    (mcu', mcuContext') = runState (platform meta.mcu) mempty
    (implementation', implementationContext') = runReader (runStateT implementation mempty) domain'

    (Context inclModule inits tasks _ bodies) =
        mcuContext'
            <> domainContext'
            <> implementationContext'

    bodyNames = nub $ fst <$> bodies

    multiBodyFunctions = mkMultiBodyFunction <$> bodyNames

    mkMultiBodyFunction :: String -> Def ('[] :-> ())
    mkMultiBodyFunction name' = proc name' $ body $ mapM_ snd $ filter (\(id', _) -> id' == name') bodies

    loop = mkLoop (systemClock mcu') tasks

    initialize :: Def ('[] :-> ())
    initialize = proc "init" $ body $ mapM_ call_ inits

    main :: Def ('[] :-> Sint32)
    main = proc "main" $ body do
        call_ initialize
        call_ loop
        ret 0

    version' = bimap fromIntegral fromIntegral meta.version

generate :: ModuleDef -> String -> String -> IO ()
generate moduleDef path name =
    runCompiler
        [package name moduleDef]
        []
        initialOpts
            { outDir = Just $ "c99" </> path
            , constFold = True
            }

build :: (Shake c) => c -> Formula p -> String -> String -> IO String
build config formula path name = do
    generate (cook formula) path name
    shake config path

mkFormula :: (Compiler c p, Shake c) => (Formula p -> Int -> Int -> c) -> Formula p -> IO ()
mkFormula mkCompiler f@Formula{..} =
    void $ build compiler f path name
  where
    startFirmware = startFlash meta.mcu
    maxLength = sizeFlash meta.mcu
    compiler = mkCompiler f startFirmware maxLength
    name = mkName meta
    path = "firmware" </> name
