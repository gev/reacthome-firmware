module Build.Compiler.GCC.GD32F4xx where

import Build.Compiler
import Build.Compiler.GCC.Config
import Core.Formula (
  Formula (Formula),
  mcu,
  quartzFrequency,
  systemFrequency,
 )
import Data.Char
import Device.GD32F4xx
import Interface.MCU

instance Compiler GCC GD32F4xx where
  mkCompiler f@Formula{mcu, quartzFrequency, systemFrequency} =
    GCC
      { path = model mcu <> modification mcu
      , defs =
          ("-D" <>)
            <$> [ toUpper <$> model mcu
                , "USE_STDPERIPH_DRIVER"
                , "USE_EXTERNPHY_LIB"
                ]
              <> sysClockDefs quartzFrequency systemFrequency
      , incs =
          ("-I" <>)
            <$> [ "support/inc"
                , "support/CMSIS/inc"
                , "support/device/gd32f4xx/inc"
                , "support/device/gd32f4xx/peripherals/inc"
                , "support/device/gd32f4xx/lwip_port"
                , "support/device/gd32f4xx/lwip_port/arch"
                , "support/device/gd32f4xx/lwip_port/Basic"
                , "support/device/ksz8091"
                , "support/lwip-2.1.2/src/include"
                ]
      , libs =
          [ "support/device/gd32f4xx"
          , "support/lwip-2.1.2/src"
          ]
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
          , "-O3"
          ]
      , -- , ld      = "-Tsupport/device/gd32f4xx/gd32f450-470.ld"
        ld = "-Tsupport/device/gd32f4xx/" <> model mcu <> modification mcu <> ".ld"
      , ldflags =
          [ "-mthumb"
          , "-mcpu=cortex-m4"
          , "-mfpu=fpv4-sp-d16"
          , "-mfloat-abi=hard"
          , "-Wl,--gc-sections"
          , "-flto"
          , "-specs=nano.specs"
          ]
      }

sysClockDefs :: Int -> Int -> [String]
sysClockDefs 25_000_000 200_000_000 =
  [ "HXTAL_VALUE=((uint32_t)25000000)"
  , "__SYSTEM_CLOCK_200M_PLL_25M_HXTAL=(uint32_t)(200000000)"
  ]
sysClockDefs 24_000_000 192_000_000 =
  [ "HXTAL_VALUE=((uint32_t)24000000)"
  , "__SYSTEM_CLOCK_192M_PLL_24M_HXTAL=(uint32_t)(192000000)"
  ]
sysClockDefs _ _ = error "Unsupported clock configuration"
