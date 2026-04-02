{-# OPTIONS_GHC -Wno-unused-imports #-}

module Build.Compiler.GCC where

import Build.Compiler
import Build.Compiler.GCC.Config
import Build.Compiler.GCC.GD32F3x0
import Build.Compiler.GCC.GD32F4xx
import Build.Shake
import Core.Formula
import Data.Digest.Pure.MD5 (md5)
import Data.String
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Util

gcc :: (Compiler GCC p) => Formula p -> Int -> Int -> GCC
gcc = mkCompiler

cc :: String
cc = "arm-none-eabi-gcc"

oc :: String
oc = "arm-none-eabi-objcopy"

dropDirectory2 :: FilePath -> FilePath
dropDirectory2 = dropDirectory1 . dropDirectory1

dropDirectory3 :: FilePath -> FilePath
dropDirectory3 = dropDirectory1 . dropDirectory1 . dropDirectory1

source :: FilePath -> FilePath
source = dropDirectory3 . dropExtension

instance Shake GCC where
    hash GCC{..} =
        show . md5 . fromString $ mconcat cflags <> mconcat defs

    shake c@GCC{..} cPath = do
        let build = "build" </> buildPath </> hash c
            dist = "dist"
            path = dist </> cPath <.> "hex"

        shakeArgs shakeOptions{shakeFiles = build} do
            want [path]

            phony "clean" do
                putInfo $ "Cleaning files in " <> build
                removeFilesAfter build ["//*"]

            dist
                <> "//*.hex" %> \out -> do
                    let elf = build </> "c99" </> cPath </> dropDirectory2 out -<.> "elf"
                    need [elf]
                    cmd_ oc "-O ihex" elf out

            build
                <> "//*.elf" %> \out -> do
                    let go lib = do
                            ss <- getDirectoryFiles lib ["//*.c", "//*.s"]
                            pure [build </> lib </> s <.> "o" | s <- ss]
                    os' <- concat <$> mapM go libs
                    let os = out -<.> "c" <.> "o" : os'
                    need os
                    cmd_ cc ldflags ld os "-lc" "-o" out

            build
                <> "//*.c.o" %> \out -> do
                    let m = out -<.> "m"
                    let a = out -<.> "asm"
                    cmd_ cc cflags defs incs "-c" (source out) "-S" "-o" a
                    cmd_ cc cflags defs incs "-c" (source out) "-o" out "-MMD -MF" m
                    neededMakefileDependencies m

            build
                <> "//*.s.o" %> \out ->
                    cmd_ cc cflags defs incs "-c" (source out) "-o" out

        pure path
