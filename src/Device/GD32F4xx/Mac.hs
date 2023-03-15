{-# LANGUAGE DataKinds #-}

module Device.GD32F4xx.Mac where


import           Data.Serialize
import           Interface.Mac
import           Ivory.Language
import           Support.Device.GD32F4xx.DBG

makeMac :: String -> Mac
makeMac = mac $ \buff -> do
    let buff' = addrOf buff
    packBE buff' 0 (0x014a :: Uint16)
    packBE buff' 2 =<< getID
