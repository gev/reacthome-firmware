{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Build.Compiler.GCC.GD32F3x0 where

import           Build.Compiler
import           Build.Compiler.GCC.Config
import           Data.Char
import           Device.GD32F3x0
import           Interface.MCU


instance Compiler GCC GD32F3x0 where

  mkCompiler MCUmod{..} =

    GCC { defs    = [ "-D" <> (toUpper <$> model)
                    , "-DUSE_STDPERIPH_DRIVER"
                    ]

        , incs    = [ "-Isupport/inc"
                    , "-Isupport/CMSIS/inc"
                    , "-Isupport/device/gd32f3x0/inc"
                    , "-Isupport/device/gd32f3x0/peripherals/inc"
                    ]

        , libs    = "support/device/gd32f3x0"

        , cflags  = [ "-mthumb"
                    , "-mcpu=cortex-m4"
                    , "-mfpu=fpv4-sp-d16"
                    , "-mfloat-abi=hard"
                    , "-ffast-math"
                    , "-fno-math-errno"
                    -- , "-fno-builtin"
                    -- , "-fno-strict-aliasing"
                    , "-fdata-sections"
                    , "-fms-extensions"
                    , "-ffunction-sections"
                    , "-Wall"
                    , "-O3"
                    ]

        , ld      = "-Tsupport/device/gd32f3x0/gd32f3x0.ld"

        , ldflags = [ "-mthumb"
                    , "-mcpu=cortex-m4"
                    , "-mfpu=fpv4-sp-d16"
                    , "-mfloat-abi=hard"
                    , "-Wl,--gc-sections"
                    , "-flto"
                    , "-specs=nano.specs"
                    ]
        }
