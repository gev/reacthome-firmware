module Core.Models where

deviceTypeUnknown = 0x00 :: Int

{-
    Legacy device types based on the PIC18 MCU family
-}

deviceTypeSensor4 = 0x01 :: Int
deviceTypeSensor6 = 0x02 :: Int
deviceTypeThi = 0x03 :: Int
deviceTypeDoppler' = 0x04 :: Int
deviceTypeRs485 = 0x06 :: Int
deviceTypeIr6 = 0x07 :: Int
deviceTypeDi16 = 0x08 :: Int
deviceTypeDi32 = 0x09 :: Int
deviceTypeDo8 = 0x0A :: Int
deviceTypeDo16 = 0x0B :: Int
deviceTypeDi16Do8 = 0x0C :: Int
deviceTypeDo8Di16 = 0x0D :: Int
deviceTypeDim4' = 0x0e :: Int
deviceTypeDim8' = 0x0f :: Int
deviceTypeIrReceiver = 0x10 :: Int
deviceTypeDo12 = 0x11 :: Int
deviceTypeDi24 = 0x12 :: Int
deviceTypeIr1 = 0x14 :: Int

{-
    The field devices
    Legacy device types based on the PIC32 MCU family
    Modern device types based on the GD32 MCU family
-}

deviceTypeDi4 = 0x20 :: Int
deviceTypeClimate = 0x21 :: Int
deviceTypeDoppler = 0x22 :: Int
deviceTypeRelay2 = 0x23 :: Int
deviceTypeIr4 = 0x24 :: Int
deviceTypeSmart4g = 0x25 :: Int
deviceTypeSmart4gd = 0x26 :: Int
deviceTypeSmart4a = 0x27 :: Int
deviceTypeSmart4am = 0x2A :: Int
deviceTypeCo2 = 0x2B :: Int
deviceTypeSmart6Push = 0x2C :: Int
deviceTypeDoppler1Di4 = 0x2D :: Int
deviceTypeDoppler5Di4 = 0x2E :: Int
deviceTypeDi4Rsm = 0x2F :: Int
deviceTypeDi4La = 0x40 :: Int
deviceTypeMixH = 0x41 :: Int
deviceTypeMixV = 0x42 :: Int
deviceTypeAo4 = 0x43 :: Int

{-
    Modern two component SMART devices
-}

deviceTypeSmartTopA6P = 0x30 :: Int
deviceTypeSmartTopG4D = 0x31 :: Int
deviceTypeSmartTopA4T = 0x32 :: Int
deviceTypeSmartTopA6T = 0x33 :: Int
deviceTypeSmartTopG6 = 0x34 :: Int
deviceTypeSmartTopG4 = 0x35 :: Int
deviceTypeSmartTopG2 = 0x36 :: Int
deviceTypeSmartTopA4P = 0x37 :: Int
deviceTypeSmartTopA4TD = 0x38 :: Int
deviceTypeSmartTopA4TD7S = 0x39 :: Int
deviceTypeSmartBottom = 0x3a :: Int
deviceTypeSmartBottomCO2 = 0x3b :: Int
deviceTypeSmartTopClimate = 0x3c :: Int
deviceTypeSmartBottomClimate = 0x3d :: Int

{-
    The DIN rail devices
    Legacy device types based on the PIC32 MCU family
    Modern device types based on the GD32 MCU family
    And one based on the Raspberry Pi
-}

deviceTypeRelay6 = 0xA0 :: Int
deviceTypeRelay12 = 0xA1 :: Int
deviceTypeRelay24 = 0xA2 :: Int
deviceTypeDim4 = 0xA3 :: Int
deviceTypeDim8 = 0xA4 :: Int
deviceTypeLanAmp = 0xA5 :: Int
deviceTypeRshub = 0xA6 :: Int
deviceTypeRelay2Din = 0xA7 :: Int
deviceTypeDi8Din = 0xA8 :: Int
deviceTypeAo4Din = 0xA9 :: Int
deviceTypeMix2 = 0xAA :: Int
deviceTypeMix1 = 0xAB :: Int
deviceTypeMix1Rs = 0xAC :: Int
deviceTypeDim12LedR = 0xAD :: Int
deviceTypeRelay12Rs = 0xAE :: Int
deviceTypeDim8Rs = 0xAF :: Int
deviceTypeRsHub1Rs = 0xB0 :: Int
deviceTypeRsHub1 = 0xB1 :: Int
deviceTypeRsHub4Old = 0xB2 :: Int

deviceTypeDim12AcRs = 0xB3 :: Int
deviceTypeDim12DcRs = 0xB4 :: Int
deviceTypeMix6x12Rs = 0xB5 :: Int
deviceTypeDim1AcRs = 0xB6 :: Int

deviceTypeServer = 0xC0 :: Int
deviceTypeRsHub4 = 0xC1 :: Int
deviceTypeSoundbox = 0xC2 :: Int

{-
    Legacy device types based on the PLC Owen family
-}
deviceTypeTemperatureExt = 0xF0 :: Int
deviceTypePlc = 0xFE :: Int

{-
  Legacy common device type with a bootloader
  based on the PIC18 MCU family
-}
deviceTypeBootloader = 0xFF :: Int
