{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Protocol.RBUS where

import           Include
import           Ivory.Language
import           Ivory.Stdlib
import           Util.CRC16
import           Util.Data.Class
import           Util.Data.Record
import           Util.Data.Value


broadcastAddress          = 0xff

preambleDiscoveryRequest  = 0x55
preambleDiscoveryResponse = 0xaa

preambleMasterPing        = 0xcc
preambleMasterConfirm     = 0xaf
preambleMasterData        = 0xa0

preambleSlavePing         = 0x33
preambleSlaveConfirm      = 0x5f
preambleSlaveData         = 0x50


data RBUS' = RBUS'
    { state :: Value Uint32
    , crc   :: Record CRC16
    }

rbus' name = RBUS'
    { state = value  (name <> "_state") 0
    , crc   = record (name <> "_crc") initCRC16
    }

instance Include RBUS' where
  include (RBUS' state crc) =
    include state >> include crc

x = rbus' "slave"

c =  crc x

a = c <| msb $ 0

b = c |> msb
