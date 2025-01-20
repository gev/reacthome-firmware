module Core.Models where

import           Ivory.Language


deviceTypeUnknown       = 0x00 :: Uint8



{-
    Legacy device types based on the PIC18 MCU family
-}

deviceTypeSensor4       = 0x01 :: Uint8
deviceTypeSensor6       = 0x02 :: Uint8
deviceTypeThi           = 0x03 :: Uint8
deviceTypeDoppler'      = 0x04 :: Uint8
deviceTypeRs485         = 0x06 :: Uint8
deviceTypeIr6           = 0x07 :: Uint8
deviceTypeDi16          = 0x08 :: Uint8
deviceTypeDi32          = 0x09 :: Uint8
deviceTypeDo8           = 0x0A :: Uint8
deviceTypeDo16          = 0x0B :: Uint8
deviceTypeDi16Do8       = 0x0C :: Uint8
deviceTypeDo8Di16       = 0x0D :: Uint8
deviceTypeDim4'         = 0x0e :: Uint8
deviceTypeDim8'         = 0x0f :: Uint8
deviceTypeIrReceiver    = 0x10 :: Uint8
deviceTypeDo12          = 0x11 :: Uint8
deviceTypeDi24          = 0x12 :: Uint8
deviceTypeIr1           = 0x14 :: Uint8



{-
    The field devices
    Legacy device types based on the PIC32 MCU family
    Modern device types based on the GD32 MCU family
-}

deviceTypeDi4            = 0x20 :: Uint8
deviceTypeClimate        = 0x21 :: Uint8
deviceTypeDoppler        = 0x22 :: Uint8
deviceTypeRelay2         = 0x23 :: Uint8
deviceTypeIr4            = 0x24 :: Uint8
deviceTypeSmart4g        = 0x25 :: Uint8
deviceTypeSmart4gd       = 0x26 :: Uint8
deviceTypeSmart4a        = 0x27 :: Uint8
deviceTypeSmart4am       = 0x2A :: Uint8
deviceTypeCo2            = 0x2B :: Uint8
deviceTypeSmart6Push     = 0x2C :: Uint8
deviceTypeDoppler1Di4    = 0x2D :: Uint8
deviceTypeDoppler5Di4    = 0x2E :: Uint8
deviceTypeDi4Rsm         = 0x2F :: Uint8
deviceTypeDi4La           = 0x40 :: Uint8



{-
    Modern two component SMART devices
-}

deviceTypeSmartTopA6P   = 0x30 :: Uint8
deviceTypeSmartTopG4D   = 0x31 :: Uint8
deviceTypeSmartTopA4T   = 0x32 :: Uint8
deviceTypeSmartTopA6T   = 0x33 :: Uint8
deviceTypeSmartTopG6    = 0x34 :: Uint8
deviceTypeSmartTopG4    = 0x35 :: Uint8
deviceTypeSmartTopG2    = 0x36 :: Uint8
deviceTypeSmartTopA4P   = 0x37 :: Uint8
deviceTypeSmartTopA4TD  = 0x38 :: Uint8

deviceTypeSmartBottom1  = 0x3a :: Uint8
deviceTypeSmartBottom2  = 0x3b :: Uint8



{-
    The DIN rail devices
    Legacy device types based on the PIC32 MCU family
    Modern device types based on the GD32 MCU family
    And one based on the Raspberry Pi
-}

deviceTypeRelay6        = 0xA0 :: Uint8
deviceTypeRelay12       = 0xA1 :: Uint8
deviceTypeRelay24       = 0xA2 :: Uint8
deviceTypeDim4          = 0xA3 :: Uint8
deviceTypeDim8          = 0xA4 :: Uint8
deviceTypeLanAmp        = 0xA5 :: Uint8
deviceTypeRshub         = 0xA6 :: Uint8
deviceTypeRelay2Din     = 0xA7 :: Uint8
deviceTypeDi8Din        = 0xA8 :: Uint8
deviceTypeAo4Din        = 0xA9 :: Uint8
deviceTypeMix2          = 0xAA :: Uint8
deviceTypeMix1          = 0xAB :: Uint8
deviceTypeMix1Rs        = 0xAC :: Uint8
deviceTypeDim12LedR     = 0xAD :: Uint8
deviceTypeRelay12Rs     = 0xAE :: Uint8
deviceTypeDim8Rs        = 0xAF :: Uint8
deviceTypeRsHub1Rs      = 0xB0 :: Uint8
deviceTypeRsHub1        = 0xB1 :: Uint8
deviceTypeRsHub4Old     = 0xB2 :: Uint8

deviceTypeDim12AcRs     = 0xB3 :: Uint8
deviceTypeDim12DcRs     = 0xB4 :: Uint8
deviceTypeMix6x12Rs     = 0xB5 :: Uint8
deviceTypeDim1AcRs      = 0xB6 :: Uint8

deviceTypeServer        = 0xC0 :: Uint8
deviceTypeRsHub4        = 0xC1 :: Uint8



{-
    Legacy device types based on the PLC Owen family
-}
deviceTypeTemperatureExt= 0xF0 :: Uint8
deviceTypePlc           = 0xFE :: Uint8



{-
  Legacy common device type with a bootloader
  based on the PIC18 MCU family
-}
deviceTypeBootloader    = 0xFF :: Uint8
