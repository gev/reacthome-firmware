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
                    , "-Isupport/device/gd32f4xx/ksz8091"
                    , "-Isupport/device/gd32f4xx/peripherals/inc"
                    , "-Isupport/device/gd32f4xx/lwip_port/Basic"			
                    , "-Isupport/device/gd32f4xx/lwip_port/arch"			
                    , "-Isupport/device/gd32f4xx/lwip_port"					
                    , "-Isupport/lwip-2.1.2/src/include/compat/posix/arpa"	
                    , "-Isupport/lwip-2.1.2/src/include/compat/posix/net"		
                    , "-Isupport/lwip-2.1.2/src/include/compat/posix"			
                    , "-Isupport/lwip-2.1.2/src/include"						
                    , "-Isupport/lwip-2.1.2/src/include/lwip"					
                    , "-Isupport/lwip-2.1.2/src/include/netif"				
                    , "-Isupport/lwip-2.1.2/src/include/netif/ppp"			
                    , "-Isupport/lwip-2.1.2/src/include/netif/ppp/polarssl"	 
                    ]

        , libs    = [ "support/device/gd32f4xx"
                    , "support/lwip-2.1.2/src"
                    ]

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
