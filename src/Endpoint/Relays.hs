{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Endpoint.Relays where

import           Core.Context
import           Data.Buffer
import           Data.Record
import           Data.Serialize
import           GHC.TypeNats
import           Ivory.Language



type RelayStruct = "relay_struct"

[ivory|
    struct relay_struct
    { state           :: IBool
    ; defaultDelayOff :: Uint32
    ; delayOff        :: Uint32
    ; delayOn         :: Uint32
    ; timestamp       :: Uint32
    ; group           :: Uint8
    ; synced          :: IBool
    }
|]



data Relays = Relays
    { runRelays :: RunRecords RelayStruct
    , payload   :: Buffer 8 Uint8
    }

relays :: String -> Int -> Relays
relays name n = Relays
    { runRelays = runRecords name $ replicate n go
    , payload   = buffer "relay_message"
    } where go = [ state           .= ival false
                 , defaultDelayOff .= ival 0
                 , delayOff        .= ival 0
                 , delayOn         .= ival 0
                 , timestamp       .= ival 0
                 , group           .= ival 1
                 , synced          .= ival true
                 ]



message :: Relays -> Uint8 -> Ivory eff (Buffer 8 Uint8)
message (Relays {..}) i = do
    let payload' = addrOf payload
    runRelays $ \r -> do
        let relay = addrOf r ! toIx i
        pack   payload' 0 (0 :: Uint8)
        pack   payload' 1 $ i + 1
        pack   payload' 2 =<< deref (relay ~> state)
        pack   payload' 3 =<< deref (relay ~> group)
        packLE payload' 4 =<< deref (relay ~> defaultDelayOff)
    pure payload



instance KnownNat n => Include (Records n RelayStruct) where
    include r = do
        include $ defStruct (Proxy :: Proxy RelayStruct)
        include $ defMemArea r

instance Include Relays where
    include (Relays {..}) =
        runRelays include >> include payload
