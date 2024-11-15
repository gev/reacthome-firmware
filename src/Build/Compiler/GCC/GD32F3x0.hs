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

    GCC { path    = model

        , defs    = ("-D" <>) <$> [ (toUpper <$> model)
                                  , "USE_STDPERIPH_DRIVER"
                                  ] 

        , incs    = ("-I" <>) <$> [ "support/inc"
                                  , "support/CMSIS/inc"
                                  , "support/device/gd32f3x0/inc"
                                  , "support/device/gd32f3x0/peripherals/inc"
                                  ]

        , libs    = [ "support/device/gd32f3x0" ]

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
                    , "-Os"
                    ]

        , ld      = "-Tsupport/device/gd32f3x0/gd32f3x0.ld"

        , ldflags = [ "-lm", "-lc", "-lgcc"
                    , "-mthumb"
                    , "-mcpu=cortex-m4"
                    , "-mfpu=fpv4-sp-d16"
                    , "-mfloat-abi=hard"
                    , "-Wl,--gc-sections"
                    , "-flto"
                    , "-specs=nano.specs"
                    ]
        }
