{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

module Build.Compiler.GCC.GD32F3x0 where

import Build.Compiler
import Build.Compiler.GCC.Config
import Core.Formula (
  Formula (Formula),
  mcu,
  quartzFrequency,
  systemFrequency,
 )
import Data.Char
import Device.GD32F3x0
import Interface.MCU

instance Compiler GCC GD32F3x0 where
  mkCompiler Formula{mcu, quartzFrequency, systemFrequency} =
    GCC
      { path = model mcu <> modification mcu
      , defs =
          ("-D" <>)
            <$> [ toUpper <$> model mcu
                , "USE_STDPERIPH_DRIVER"
                ]
              <> sysClockDefs quartzFrequency systemFrequency
      , incs =
          ("-I" <>)
            <$> [ "support/inc"
                , "support/CMSIS/inc"
                , "support/device/gd32f3x0/inc"
                , "support/device/gd32f3x0/peripherals/inc"
                ]
      , libs = ["support/device/gd32f3x0"]
      , cflags =
          [ "-mthumb"
          , "-mcpu=cortex-m4"
          , "-mfpu=fpv4-sp-d16"
          , "-mfloat-abi=hard"
          , "-ffast-math"
          , "-fno-math-errno"
          , -- , "-fno-builtin"
            -- , "-fno-strict-aliasing"
            "-fdata-sections"
          , -- , "-fms-extensions"
            "-ffunction-sections"
          , "-Wall"
          , "-Wno-main"
          , "-Os"
          ]
      , ld = "-Tsupport/device/gd32f3x0/gd32f3x0.ld"
      , ldflags =
          [ "-lm"
          , "-lc"
          , "-lgcc"
          , "-mthumb"
          , "-mcpu=cortex-m4"
          , "-mfpu=fpv4-sp-d16"
          , "-mfloat-abi=hard"
          , "-Wl,--gc-sections"
          , "-flto"
          , "-specs=nano.specs"
          ]
      }

sysClockDefs :: Int -> Int -> [String]
sysClockDefs 8_000_000 84_000_000 =
  [ "HXTAL_VALUE=((uint32_t)8000000)"
  , "__SYSTEM_CLOCK_84M_PLL_HXTAL=((uint32_t)84000000)"
  ]
sysClockDefs _ _ = error "Unsupported clock configuration"
