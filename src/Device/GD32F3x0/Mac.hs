{-# LANGUAGE DataKinds #-}

module Device.GD32F3x0.Mac where


import           Control.Monad
import           Data.Serialize
import           GHC.TypeNats
import           Interface.Mac
import           Ivory.Language
import           Support.Device.GD32F3x0.DBG

makeMac :: String -> Mac
makeMac = mac inclDBG $ \buff -> do
    let buff' = addrOf buff
    packBE buff' 0 (0x014a :: Uint16)
    packBE buff' 2 =<< getID
