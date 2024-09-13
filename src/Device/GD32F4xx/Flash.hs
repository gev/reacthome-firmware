{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Device.GD32F4xx.Flash where


import           Interface.Flash
import           Ivory.Language
import           Support.Cast
import           Support.Device.GD32F4xx.FmcOperation.FmcOperation



newtype PageAddr = PageAddr {getAddr :: Uint32}
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar, Num)


mkPage :: Uint32 -> PageAddr
mkPage =  PageAddr



instance Flash PageAddr where
    address (PageAddr page) (Addr offset) = page + offset

    write page offset value = do
        write32Bit (address page offset) value

    erasePage page offset = do
        eraseSector (address page offset)

    read page offset =
        derefUint32 $ address page offset
