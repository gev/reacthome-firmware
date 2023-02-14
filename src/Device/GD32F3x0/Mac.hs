{-# LANGUAGE DataKinds #-}

module Device.GD32F3x0.Mac where


import           Control.Monad
import           GHC.TypeNats
import           Interface.Mac
import           Ivory.Language
import           Support.Device.GD32F3x0.DBG
import           Util.ByteSplit
import           Util.Data.Class

systemMac :: String -> Mac
systemMac = mac inclDBG $ \buff -> do
    id <- getID
    zipWithM_ (setItem buff)
              (iterate (+1) 0)
            $ 0x01 : 0x4a : bytes id
