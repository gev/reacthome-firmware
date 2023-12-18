module Core.Models where

import Ivory.Language

deviceTypeUnknown       = 0x00 :: Uint8

deviceTypeDi4           = 0x20 :: Uint8
deviceTypeClimate       = 0x21 :: Uint8
deviceTypeDoppler       = 0x22 :: Uint8
deviceTypeRelay2        = 0x23 :: Uint8
deviceTypeIr4           = 0x24 :: Uint8
deviceTypeSmart4g       = 0x25 :: Uint8
deviceTypeSmart4gd      = 0x26 :: Uint8
deviceTypeSmart4a       = 0x27 :: Uint8
deviceTypeDi4W          = 0x28 :: Uint8
deviceTypeIbutton       = 0x29 :: Uint8
deviceTypeSmart4am      = 0x2A :: Uint8
deviceTypeCo2           = 0x2B :: Uint8
deviceTypeSmart6Push    = 0x2C :: Uint8

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

deviceTypeServer        = 0xC0 :: Uint8
deviceTypeRsHub4        = 0xC1 :: Uint8
