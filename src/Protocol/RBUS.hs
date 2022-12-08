{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Protocol.RBUS where

import           Ivory.Language
import           Ivory.Stdlib

type Effect e = forall eff. Ivory (ProcEffects eff ()) e

type Data  = forall s. Effect (Ref s (Array 512 (Stored Uint8)))
type Byte  = Effect Uint8

data Fragment = Fragment
  { array  :: Effect Data
  , offset :: Effect Uint16
  , length :: Effect Uint16
  }

data Packet
  = DiscoveryRequest  { mac          :: Fragment
                      , deviceType   :: Byte
                      , versionMajor :: Byte
                      , versionMinor :: Byte
                      }
  | DiscoveryResponse { mac     :: Fragment
                      , address :: Byte
                      }
  | Ping              { address :: Byte
                      }
  | Confirm           { address :: Byte
                      }
  | Data              { id      :: Byte
                      , address :: Byte
                      , message :: Fragment
                      }

broadcastAddress          = 0xff

preambleDiscoveryRequest  = 0x55
preambleDiscoveryResponse = 0xaa

preambleMasterPing        = 0xcc
preambleMasterConfirm     = 0xaf
preambleMasterData        = 0xa0

preambleSlavePing         = 0x33
preambleSlaveConfirm      = 0x5f
preambleSlaveData         = 0x50



parse :: Uint8 -> Maybe Packet
parse x = Just . Ping $ ifte (x ==? 1) (pure 1) (pure 0)



serialize :: Packet -> Effect Uint8
serialize (Ping x) = (+1) <$> x
