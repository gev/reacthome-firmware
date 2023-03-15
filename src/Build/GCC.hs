{-# LANGUAGE RecordWildCards #-}

module Build.GCC where

import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Util



data GCC = GCC
    { cc      ::  String
    , oc      ::  String
    , ld      ::  String
    , defs    :: [String]
    , incs    :: [String]
    , libs    ::  String
    , cflags  :: [String]
    , ldflags :: [String]
    }



-- target :: Foldable t => [FilePath] -> t String -> [FilePath]
-- target ns = concatMap $ \x -> ["build/firmware" </> n <.> x | n <- ns]

-- source :: FilePath -> FilePath
-- source = dropDirectory1 . dropExtension

-- class Shake c where
--     shake :: c -> [String] -> IO ()

-- instance Shake GCC where

--     shake (GCC {..}) ns = shakeArgs shakeOptions{shakeFiles="build"} $ do

--         want $ target ns ["hex", "bin"]

--         phony "clean" $ do
--             putInfo "Cleaning files in build"
--             removeFilesAfter "build" ["//*"]

--         "build//*.bin" %> \out -> do
--             let elf = out -<.> "elf"
--             need [elf]
--             cmd_ oc "-O binary" elf out

--         "build//*.hex" %> \out -> do
--             let elf = out -<.> "elf"
--             need [elf]
--             cmd_ oc "-O ihex" elf out

--         "build//*.elf" %> \out -> do
--             ss <- getDirectoryFiles "support/device/gd32f3x0" ["//*.c", "//*.s"]
--             ss' <- getDirectoryFiles "support/CMSIS" ["//*.c"]
--             let os = out -<.> "c" <.> "o"
--                    : ["build/support/device/gd32f3x0" </> s <.> "o" | s <- ss]
--                   <> ["build/support/CMSIS" </> s <.> "o" | s <- ss']
--             need os
--             cmd_ cc ldflags ld os "-lc" "-o" out

--         "build//*.c.o" %> \out -> do
--             let m = out -<.> "m"
--             let a = out -<.> "asm"
--             cmd_ cc cflags defs incs "-c" (source out) "-S" "-o" a
--             cmd_ cc cflags defs incs "-c" (source out) "-o" out "-MMD -MF" m
--             neededMakefileDependencies m

--         "build//*.s.o" %> \out ->
--             cmd_ cc cflags defs incs "-c" (source out) "-o" out
