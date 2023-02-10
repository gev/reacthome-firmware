{-# LANGUAGE DataKinds #-}

module Device.GD32F3x0.Mac where


import           Interface.Mac               as I
import           Ivory.Language
import           Support.Device.GD32F3x0.DBG
import           Util
import           Util.Data.Class


mac :: Mac 6 Uint8
mac = I.mac inclDBG $ \buff -> do
    id <- getID
    let
    setItem buff 0 0x01
    setItem buff 1 0x4a
    setItem buff 2 $ id .! 3
    setItem buff 3 $ id .! 2
    setItem buff 4 $ id .! 1
    setItem buff 5 $ id .! 0
