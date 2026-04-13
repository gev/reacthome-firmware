module Core.Models where

import Data.Word

deviceTypeUnknown = 0x00 :: Word16

{-
    Legacy device types based on the PIC18 MCU family
-}

deviceTypeSensor4 = 0x01 :: Word16
deviceTypeSensor6 = 0x02 :: Word16
deviceTypeThi = 0x03 :: Word16
deviceTypeDoppler' = 0x04 :: Word16
deviceTypeRs485 = 0x06 :: Word16
deviceTypeIr6 = 0x07 :: Word16
deviceTypeDi16 = 0x08 :: Word16
deviceTypeDi32 = 0x09 :: Word16
deviceTypeDo8 = 0x0A :: Word16
deviceTypeDo16 = 0x0B :: Word16
deviceTypeDi16Do8 = 0x0C :: Word16
deviceTypeDo8Di16 = 0x0D :: Word16
deviceTypeDim4' = 0x0e :: Word16
deviceTypeDim8' = 0x0f :: Word16
deviceTypeIrReceiver = 0x10 :: Word16
deviceTypeDo12 = 0x11 :: Word16
deviceTypeDi24 = 0x12 :: Word16
deviceTypeIr1 = 0x14 :: Word16

{-
    The field devices
    Legacy device types based on the PIC32 MCU family
    Modern device types based on the GD32 MCU family
-}

deviceTypeDi4 = 0x20 :: Word16
deviceTypeClimate = 0x21 :: Word16
deviceTypeDoppler = 0x22 :: Word16
deviceTypeRelay2 = 0x23 :: Word16
deviceTypeIr4 = 0x24 :: Word16
deviceTypeSmart4g = 0x25 :: Word16
deviceTypeSmart4gd = 0x26 :: Word16
deviceTypeSmart4a = 0x27 :: Word16
deviceTypeSmart4am = 0x2A :: Word16
deviceTypeCo2 = 0x2B :: Word16
deviceTypeSmart6Push = 0x2C :: Word16
deviceTypeDoppler1Di4 = 0x2D :: Word16
deviceTypeDoppler5Di4 = 0x2E :: Word16
deviceTypeDi4Rsm = 0x2F :: Word16
deviceTypeDi4La = 0x40 :: Word16
deviceTypeMixH = 0x41 :: Word16
deviceTypeMixV = 0x42 :: Word16
deviceTypeAo4 = 0x43 :: Word16
deviceTypeCardHolder = 0x44 :: Word16

{-
    Modern two component SMART devices
-}

deviceTypeSmartTopA6P = 0x30 :: Word16
deviceTypeSmartTopG4D = 0x31 :: Word16
deviceTypeSmartTopA4T = 0x32 :: Word16
deviceTypeSmartTopA6T = 0x33 :: Word16
deviceTypeSmartTopG6 = 0x34 :: Word16
deviceTypeSmartTopG4 = 0x35 :: Word16
deviceTypeSmartTopG2 = 0x36 :: Word16
deviceTypeSmartTopA4P = 0x37 :: Word16
deviceTypeSmartTopA4TD = 0x38 :: Word16
deviceTypeSmartTopA4TD7S = 0x39 :: Word16
deviceTypeSmartBottom = 0x3a :: Word16
deviceTypeSmartBottomCO2 = 0x3b :: Word16
deviceTypeSmartTopClimate = 0x3c :: Word16
deviceTypeSmartBottomClimate = 0x3d :: Word16

{-
    The DIN rail devices
    Legacy device types based on the PIC32 MCU family
    Modern device types based on the GD32 MCU family
    And one based on the Raspberry Pi
-}

deviceTypeRelay6 = 0xA0 :: Word16
deviceTypeRelay12 = 0xA1 :: Word16
deviceTypeRelay24 = 0xA2 :: Word16
deviceTypeDim4 = 0xA3 :: Word16
deviceTypeDim8 = 0xA4 :: Word16
deviceTypeLanAmp = 0xA5 :: Word16
deviceTypeRshub = 0xA6 :: Word16
deviceTypeRelay2Din = 0xA7 :: Word16
deviceTypeDi8Din = 0xA8 :: Word16
deviceTypeAo4Din = 0xA9 :: Word16
deviceTypeMix2 = 0xAA :: Word16
deviceTypeMix1 = 0xAB :: Word16
deviceTypeMix1Rs = 0xAC :: Word16
deviceTypeDim12LedR = 0xAD :: Word16
deviceTypeRelay12Rs = 0xAE :: Word16
deviceTypeDim8Rs = 0xAF :: Word16
deviceTypeRsHub1Rs = 0xB0 :: Word16
deviceTypeRsHub1 = 0xB1 :: Word16
deviceTypeRsHub4Old = 0xB2 :: Word16

deviceTypeDim12AcRs = 0xB3 :: Word16
deviceTypeDim12DcRs = 0xB4 :: Word16
deviceTypeMix6x12Rs = 0xB5 :: Word16
deviceTypeDim1AcRs = 0xB6 :: Word16

deviceTypeServer = 0xC0 :: Word16
deviceTypeRsHub4 = 0xC1 :: Word16
deviceTypeSoundbox = 0xC2 :: Word16

{-
    Legacy device types based on the PLC Owen family
-}
deviceTypeTemperatureExt = 0xF0 :: Word16
deviceTypePlc = 0xFE :: Word16

{-
  Legacy common device type with a bootloader
  based on the PIC18 MCU family
-}
deviceTypeBootloader = 0xFF :: Word16
