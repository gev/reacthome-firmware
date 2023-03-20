{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Build.Compiler.GCC.GD32F4xx where

import           Build.Compiler
import           Build.Compiler.GCC.Config
import           Data.Char
import           Device.GD32F4xx
import           Interface.MCU


instance Compiler GCC GD32F4xx where

  makeConfig (MCU {..}) =

    GCC { defs    = [ "-D" <> (toUpper <$> model)
                    , "-DUSE_STDPERIPH_DRIVER"
                    ]

        , incs    = [ "-Isupport/inc"
                    , "-Isupport/CMSIS/inc"
                    , "-Isupport/device/gd32f4xx/inc"
                    , "-Isupport/device/gd32f4xx/peripherals/inc"
                    ]

        , libs    = "support/device/gd32f4xx"

        , cflags  = [ "-mthumb"
                    , "-mcpu=cortex-m4"
                    , "-mfloat-abi=soft"
                    , "-fno-builtin"
                    , "-fno-strict-aliasing"
                    , "-fdata-sections"
                    , "-fms-extensions"
                    , "-ffunction-sections"
                    , "-Wall"
                    , "-O3"
                    ]

        , ld      = "-Tsupport/device/gd32f4xx/" <> model <> modification <> ".ld"

        , ldflags = [ "-mthumb"
                    , "-mcpu=cortex-m4"
                    , "-mfloat-abi=soft"
                    , "-Wl,--gc-sections"
                    , "-flto"
                    , "-specs=nano.specs"
                    ]
        }
