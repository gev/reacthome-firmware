module Protocol.RS485.RBUS where

import Ivory.Language

data Preamble = Preamble
    { discovery :: Uint8
    , ping :: Uint8
    , confirm :: Uint8
    , message :: Uint8
    }

preambleMaster =
    Preamble
        { discovery = 0xaa
        , ping = 0xcc
        , confirm = 0xaf
        , message = 0xa0
        }

preambleSlave =
    Preamble
        { discovery = 0x55
        , ping = 0x33
        , confirm = 0x5f
        , message = 0x50
        }

defaultBaudrate = 1_000_000 :: Uint32

messageTTL = 16 :: Uint8

broadcastAddress = 0xff :: Uint8

readyToReceive = 0x00 :: Uint8
receivingDiscovery = 0x01 :: Uint8
receivingPing = 0x02 :: Uint8
receivingConfirm = 0x03 :: Uint8
receivingMessage = 0x04 :: Uint8
skippingAll = 0x05 :: Uint8
skippingMsg = 0x06 :: Uint8

waitingAddress = 0x00 :: Uint8
waitingTid = 0x01 :: Uint8
waitingSize = 0x02 :: Uint8
waitingMac = 0x03 :: Uint8
waitingModel = 0x04 :: Uint8
waitingMajorVersion = 0x05 :: Uint8
waitingMinorVersion = 0x06 :: Uint8
waitingData = 0x07 :: Uint8
waitingMsbCRC = 0x08 :: Uint8
waitingLsbCRC = 0x09 :: Uint8
