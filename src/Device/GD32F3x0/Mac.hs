{-# LANGUAGE DataKinds #-}

module Device.GD32F3x0.Mac where


import           Control.Monad
import           Interface.Mac
import           Ivory.Language
import           Support.Device.GD32F3x0.DBG
import           Util.ByteSplit
import           Util.Data.Class

systemMac :: Mac 6 Uint8
systemMac = mac inclDBG $ \buff -> do
    id <- getID
    zipWithM_ (setItem buff)
              [0, 1, 2, 3, 4, 5]
            $ [0x01, 0x4a] <> bytes id
