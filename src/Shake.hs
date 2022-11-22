module Shake where

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util


shake :: IO ()
shake = shakeArgs shakeOptions{shakeFiles="build"} $ do
    want ["build/blink.hex"]

    phony "clean" $ do
        putInfo "Cleaning files in build"
        removeFilesAfter "build" ["//*"]

    "build/blink.hex" %> \out -> do
        cs <- getDirectoryFiles "c" ["//*.c"]
        let os = ["build" </> c -<.> "o" | c <- cs]
        need os
        -- cmd_ "arm-none-eabi-gcc -o" [out] os

    "build//*.o" %> \out -> do
        let c = "c" </> (dropDirectory1 $ out -<.> "c")
        let m = out -<.> "m"
        cmd_ "arm-none-eabi-gcc -c" [c] "-o" [out] "-MMD -MF" [m]
             "-DGD32F330"
             "-Ibuild -Ic/support/CMSIS/inc/ -Ic/support/device/gd32f3x0/inc -Ic/support/device/gd32f3x0/peripherals/inc/"
             "-mthumb -mcpu=cortex-m4 -mfloat-abi=soft -fno-builtin -fno-strict-aliasing -fdata-sections -fms-extensions -ffunction-sections -Og"
        neededMakefileDependencies m
