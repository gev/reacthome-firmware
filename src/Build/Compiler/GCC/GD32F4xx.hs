{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Build.Compiler.GCC.GD32F4xx where

import           Build.Compiler
import           Build.Compiler.GCC.Config
import           Data.Char
import           Device.GD32F4xx
import           Interface.MCU


instance Compiler GCC GD32F4xx where

  mkCompiler MCUmod{..} =

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
                    , "-mfpu=fpv4-sp-d16"
                    , "-mfloat-abi=hard"
                    , "-ffast-math"
                    , "-fno-math-errno"
                    -- , "-fno-builtin"
                    -- , "-fno-strict-aliasing"
                    , "-fdata-sections"
                    -- , "-fms-extensions"
                    , "-ffunction-sections"
                    , "-Wall", "-Wno-main"
                    , "-O3"
                    ]

        , ld      = "-Tsupport/device/gd32f4xx/gd32f450-470.ld"
        -- , ld      = "-Tsupport/device/gd32f4xx/" <> model <> modification <> ".ld"

        , ldflags = [ "-mthumb"
                    , "-mcpu=cortex-m4"
                    , "-mfpu=fpv4-sp-d16"
                    , "-mfloat-abi=hard"
                    , "-Wl,--gc-sections"
                    , "-flto"
                    , "-specs=nano.specs"
                    ]
        }
