module Shake where

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util


cc :: String
cc = "arm-none-eabi-gcc"

oc :: String
oc ="arm-none-eabi-objcopy"

defs :: [String]
defs =
    [ "-DGD32F330"
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
    , "-mfloat-abi=soft"
    , "-fno-builtin"
    , "-fno-strict-aliasing"
    , "-fdata-sections"
    , "-fms-extensions"
    , "-ffunction-sections"
    , "-Og"
    ]

ldflags :: [String]
ldflags =
    [ "-mthumb"
    , "-mcpu=cortex-m4"
    , "-mfloat-abi=soft"
    , "-Wl,--gc-sections"
    , "-flto"
    , "-specs=nano.specs"
    ]

ld :: String
ld = "-Tsupport/device/gd32f3x0/gd32f3x0.ld"


shake :: IO ()
shake = shakeArgs shakeOptions{shakeFiles="build"} $ do
    want [ "build/firmware/blink.elf"
         , "build/firmware/scheduler.elf"
         , "build/firmware/usart.elf"
         ]

    phony "clean" $ do
        putInfo "Cleaning files in build"
        removeFilesAfter "build" ["//*"]

    "build/firmware/*.elf" %> \out -> do
        let o = out -<.> "o"
        cs <- getDirectoryFiles "support" ["//*.c"]
        need ["build/support" </> c -<.> "o" | c <- cs]
        need [o]
        cmd_ cc ldflags ld o "-lc" "-o" [out -<.> "elf"]

    "build//*.o" %> \out -> do
        let c = dropDirectory1 out -<.> "c"
        let m = out -<.> "m"
        cmd_ cc cflags defs incs "-c" c "-o" out "-MMD -MF" m
        neededMakefileDependencies m
