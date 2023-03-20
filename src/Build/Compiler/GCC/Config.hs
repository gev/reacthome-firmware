{-# LANGUAGE RecordWildCards #-}

module Build.Compiler.GCC.Config where

import           Build.Compiler
import           Build.Shake
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Util
import           Interface.MCU


cc :: String
cc = "arm-none-eabi-gcc"

oc :: String
oc = "arm-none-eabi-objcopy"



data GCC = GCC
    { defs    :: [String]
    , incs    :: [String]
    , libs    ::  String
    , cflags  :: [String]
    , ld      ::  String
    , ldflags :: [String]
    }



target :: Foldable t => [FilePath] -> t String -> [FilePath]
target ns = concatMap $ \x -> ["build/firmware" </> n <.> x | n <- ns]

source :: FilePath -> FilePath
source = dropDirectory1 . dropExtension

instance Shake GCC where

    shake (GCC {..}) ns = shakeArgs shakeOptions{shakeFiles="build"} $ do

        want $ target ns ["hex", "bin"]

        phony "clean" $ do
            putInfo "Cleaning files in build"
            removeFilesAfter "build" ["//*"]

        "build//*.bin" %> \out -> do
            let elf = out -<.> "elf"
            need [elf]
            cmd_ oc "-O binary" elf out

        "build//*.hex" %> \out -> do
            let elf = out -<.> "elf"
            need [elf]
            cmd_ oc "-O ihex" elf out

        "build//*.elf" %> \out -> do
            ss <- getDirectoryFiles libs ["//*.c", "//*.s"]
            let os = out -<.> "c" <.> "o"
                   : ["build" </> libs </> s <.> "o" | s <- ss]
            need os
            cmd_ cc ldflags ld os "-lc" "-o" out

        "build//*.c.o" %> \out -> do
            let m = out -<.> "m"
            let a = out -<.> "asm"
            cmd_ cc cflags defs incs "-c" (source out) "-S" "-o" a
            cmd_ cc cflags defs incs "-c" (source out) "-o" out "-MMD -MF" m
            neededMakefileDependencies m

        "build//*.s.o" %> \out ->
            cmd_ cc cflags defs incs "-c" (source out) "-o" out
