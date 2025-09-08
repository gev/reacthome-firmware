module Core.Actions where

import           Ivory.Language

actionDo                      = 0x00 :: Uint8
actionDi                      = 0x01 :: Uint8
actionGroup                   = 0x02 :: Uint8
actionDiRelaySync             = 0x03 :: Uint8
actionMix                     = 0x04 :: Uint8

actionVibro                   = 0x0A :: Uint8

actionIr                      = 0x10 :: Uint8
actionIrConfig                = 0x11 :: Uint8

actionLanamp                  = 0x20 :: Uint8
actionRtp                     = 0x21 :: Uint8

actionSmartTop                = 0x30 :: Uint8
actionSmartTopDetect          = 0x31 :: Uint8

actionALedOff                 = 0x40 :: Uint8
actionALedOn                  = 0x41 :: Uint8
actionALedColorAnimationPlay  = 0x42 :: Uint8
actionALedColorAnimationStop  = 0x43 :: Uint8
actionALedMaskAnimationPlay   = 0x44 :: Uint8
actionALedMaskAnimationStop   = 0x45 :: Uint8
actionALedClip                = 0x46 :: Uint8
actionALedBrightness          = 0x47 :: Uint8
actionALedConfigGroup         = 0x48 :: Uint8

actionRs485Mode               = 0xA0 :: Uint8
actionRbusTransmit            = 0xA1 :: Uint8
actionRs485Transmit           = 0xA2 :: Uint8

actionDoppler0                = 0xB0 :: Uint8  --deprecated
actionDopplerRaw              = 0xB1 :: Uint8
actionDoppler1                = 0xB2 :: Uint8

actionTemperature             = 0xC0 :: Uint8
actionTemperatureExternal0    = 0xC1 :: Uint8  --deprecated
actionHumidity                = 0xC2 :: Uint8
actionIllumination            = 0xC3 :: Uint8
actionTemperatureExternal1    = 0xC4 :: Uint8  --deprecated
actionCO2                     = 0xC5 :: Uint8
actionTemperatureExternal     = 0xC6 :: Uint8
actionTemperatureCorrection   = 0xCF :: Uint8

actionDim                     = 0xD0 :: Uint8

actionRGB                     = 0xE0 :: Uint8
actionImage                   = 0xE1 :: Uint8
actionBlink                   = 0xE2 :: Uint8
actionPalette                 = 0xE3 :: Uint8

actionDiscovery               = 0xF0 :: Uint8
actionReady                   = 0xF1 :: Uint8
actionInitialize              = 0xF2 :: Uint8
actionInitialized             = 0xF3 :: Uint8
actionGetState                = 0xF4 :: Uint8
actionLog                     = 0xF9 :: Uint8
actionFindMe                  = 0xFA :: Uint8
actionBootload                = 0xFB :: Uint8
actionMacAddress              = 0xFC :: Uint8
actionIpAddress               = 0xFD :: Uint8
actionError                   = 0xFF :: Uint8
