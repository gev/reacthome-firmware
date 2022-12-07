{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Protocol.RBUS where

import           Ivory.Language


type Data  = forall s. Ref s ('CArray (Stored Uint8))
type MAC   = forall s. Ref s ('Array 6 (Stored Uint8))

data Packet
  = MasterDiscovery { mac     :: MAC
                    , address :: Uint8
                    }
  | MasterPing      { address :: Uint8
                    }
  | MasterConfirm   { address :: Uint8
                    }
  | MasterData      { id      :: Uint8
                    , address :: Uint8
                    , message :: Data
                    , length  :: Data
                    }
  | SlaveDiscovery  { mac          :: MAC
                    , deviceType   :: Uint8
                    , versionMajor :: Uint8
                    , versionMinor :: Uint8
                    }
  | SlavePing       { address :: Uint8
                    }
  | SlaveConfig     { address :: Uint8
                    }
  | SlaveData       { id      :: Uint8
                    , address :: Uint8
                    , message :: Data
                    , length  :: Data
                    }

broadcastAddress        = 0xff

preambleMasterDiscovery = 0xaa
preambleMasterPing      = 0xcc
preambleMasterConfirm   = 0xaf
preambleMasterData      = 0xa0

preambleSlaveDiscovery  = 0x55
preambleSlavePing       = 0x33
preambleSlaveConfirm    = 0x5f
preambleSlaveData       = 0x50


parse :: Data -> Uint16 -> Ivory eff Packet
parse = undefined


serialize :: Packet -> Ivory eff (Data, Uint16)
serialize = undefined
