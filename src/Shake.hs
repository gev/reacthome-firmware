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
    [ "-Isupport/CMSIS/inc"
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
    , "-Og"
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

target ns = concatMap $ \x -> ["build/firmware" </> n <.> x | n <- ns]

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
        let o = out -<.> "o"
        cs <- getDirectoryFiles "support/device/gd32f3x0" ["//*.c"]
        let os = o : ["build/support/device/gd32f3x0" </> c -<.> "o" | c <- cs]
        need os
        need ["build/support/device/gd32f3x0/src/startup_gd32f3x0.o"]
        let m = out -<.> "m"
        cmd_ cc ldflags ld os "-lc" "-o" out

    "build/support/device/gd32f3x0/src/startup_gd32f3x0.o" %> \out -> do
        let s = dropDirectory1 out -<.> "s"
        let m = out -<.> "m"
        cmd_ cc cflags "-c" s "-o" out "-MMD -MF" m
        neededMakefileDependencies m

    "build//*.o" %> \out -> do
        let c = dropDirectory1 out -<.> "c"
        let m = out -<.> "m"
        cmd_ cc cflags defs incs "-c" c "-o" out "-MMD -MF" m
        neededMakefileDependencies m
