module Shake (Shake.shake) where

import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Util


cc :: String
cc = "arm-none-eabi-gcc"

oc :: String
oc ="arm-none-eabi-objcopy"

defs :: [String]
defs =
    [ "-DGD32F330"
    , "-DUSE_STDPERIPH_DRIVER"
    ]
incs :: [String]

incs =
    [ "-Isupport/inc"
    , "-Isupport/CMSIS/inc"
    , "-Isupport/device/gd32f3x0/inc"
    , "-Isupport/device/gd32f3x0/peripherals/inc"
    ]

cflags :: [String]
cflags =
    [ "-mthumb"
    , "-mcpu=cortex-m4"
    , "-mfloat-abi=soft" --
    , "-fno-builtin" --
    , "-fno-strict-aliasing" --
    , "-fdata-sections"
    , "-fms-extensions" --
    , "-ffunction-sections"
    -- , "-Wall"
    , "-Os"
    ]

ldflags :: [String]
ldflags =
    [ "-mthumb"
    , "-mcpu=cortex-m4"
    , "-mfloat-abi=soft" --
    , "-Wl,--gc-sections"
    , "-flto" --
    , "-specs=nano.specs"
    ]

ld :: String
ld = "-Tsupport/device/gd32f3x0/gd32f3x0.ld"

target :: Foldable t => [FilePath] -> t String -> [FilePath]
target ns = concatMap $ \x -> ["build/firmware" </> n <.> x | n <- ns]

source :: FilePath -> FilePath
source = dropDirectory1 . dropExtension

shake :: [String] -> IO ()
shake ns = shakeArgs shakeOptions{shakeFiles="build"} $ do
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
        ss <- getDirectoryFiles "support/device/gd32f3x0" ["//*.c", "//*.s"]
        ss' <- getDirectoryFiles "support/CMSIS" ["//*.c"]
        let os = out -<.> "c" <.> "o"
               : ["build/support/device/gd32f3x0" </> s <.> "o" | s <- ss]
              <> ["build/support/CMSIS" </> s <.> "o" | s <- ss']
        need os
        cmd_ cc ldflags ld os "-lc" "-o" out

    "build//*.c.o" %> \out -> do
        let m = out -<.> "m"
        cmd_ cc cflags defs incs "-c" (source out) "-o" out "-MMD -MF" m
        neededMakefileDependencies m

    "build//*.s.o" %> \out ->
        cmd_ cc cflags defs incs "-c" (source out) "-o" out
