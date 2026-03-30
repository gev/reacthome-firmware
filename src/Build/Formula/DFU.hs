module Build.Formula.DFU where

import Build.Compiler
import Build.Formula
import Build.Shake
import Core.Formula
import Core.Formula.DFU
import Interface.MCU

mkDFU :: (Compiler c p, Shake c) => (Formula p -> Int -> Int -> c) -> DFU p -> IO ()
mkDFU mkCompiler DFU{..} = do
    let f =
            Formula
                { name
                , model
                , version
                , shouldInit
                , mcu
                , quartzFrequency
                , systemFrequency
                , implementation = implementation transport
                }
    let startFirmware = startFlash mcu
    let maxLength = sizeFlash mcu
    build (mkCompiler f startFirmware maxLength) f
