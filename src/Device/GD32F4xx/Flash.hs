{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Device.GD32F4xx.Flash where


import           Interface.Flash
import           Ivory.Language
import           Support.Cast
import           Support.Device.GD32F4xx.FmcOperation.FmcOperation
import           Support.CMSIS.CoreCMFunc



newtype PageAddr = PageAddr {getAddr :: Uint32}
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar, Num)


mkPage :: Uint32 -> PageAddr
mkPage =  PageAddr



instance Flash PageAddr where
    address (PageAddr page) (Addr offset) = page + offset

    write page offset value = do
        disableIRQ
        write32Bit (address page offset) value
        enableIRQ

    erasePage page offset = do
        disableIRQ
        eraseSector (address page offset)
        enableIRQ

    read page offset =
        derefUint32 $ address page offset
