{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Build.Compiler.GCC where

import           Build.Compiler
import           Build.Compiler.GCC.Config
import           Build.Compiler.GCC.GD32F3x0
import           Build.Compiler.GCC.GD32F4xx
import           Build.Firmware
import           Build.Shake
import           Core.Formula
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Util
import           Interface.MCU



gcc :: Compiler GCC p
    => MCUmod p -> [Formula p] -> IO ()
gcc = mkGCC >>= build



mkGCC :: Compiler GCC p
      => MCUmod p -> GCC
mkGCC = mkCompiler



cc :: String
cc = "arm-none-eabi-gcc"

oc :: String
oc = "arm-none-eabi-objcopy"



target :: Foldable t => [FilePath] -> t String -> [FilePath]
target ns = concatMap $ \x -> ["build/firmware" </> n <.> x | n <- ns]

source :: FilePath -> FilePath
source = dropDirectory1 . dropExtension



instance Shake GCC where

    key GCC{..} = mconcat cflags

    shake GCC{..} ns = shakeArgs shakeOptions{shakeFiles="build"} $ do

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
            let go lib = do
                    ss <- getDirectoryFiles lib ["//*.c", "//*.s"]
                    pure ["build" </> lib </> s <.> "o" | s <- ss]
            os' <- concat <$> mapM go libs
            let os = out -<.> "c" <.> "o" : os'
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
