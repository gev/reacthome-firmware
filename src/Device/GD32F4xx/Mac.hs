{-# LANGUAGE DataKinds #-}

module Device.GD32F4xx.Mac where


import           Data.Buffer
import           Data.Serialize
import           Interface.Mac
import           Ivory.Language
import           Support.Device.GD32F4xx.DBG

makeMac :: Buffer 6 Uint8 -> Ivory eff ()
makeMac buff = do
    packBE buff 0 (0x014a :: Uint16)
    packBE buff 2 =<< getID
