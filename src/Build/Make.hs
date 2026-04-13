module Build.Make where

import Build.Compiler
import Build.Formula
import Build.Formula.DFU
import Build.Shake
import Core.Formula
import Core.Formula.DFU
import Device.GD32F3x0 (GD32F3x0)
import Device.GD32F4xx (GD32F4xx)
import Support.Device.GD32F3x0.Misc qualified as GD32F3x0
import Support.Device.GD32F4xx.Misc qualified as GD32F4xx

class Make f p where
    make ::
        (Compiler c p, Shake c) =>
        (Formula p -> Int -> Int -> c) -> f p -> IO ()

instance Make Formula p where
    make = mkFormula

instance Make DFU GD32F3x0 where
    make =
        mkDFU 0x2_000 (0, 1) $
            GD32F3x0.setVectorTableNvic GD32F3x0.nvic_vecttab_flash
                . fromIntegral

instance Make DFU GD32F4xx where
    make =
        mkDFU 0x8_000 (0, 1) $
            GD32F4xx.setVectorTableNvic GD32F4xx.nvic_vecttab_flash
                . fromIntegral
