{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

module Endpoint.Relays where

import           Core.Include
import           Data.Buffer
import           Data.Record
import           Data.Serialize
import           GHC.TypeNats
import           Ivory.Language



type RelayStruct = "relay_struct"

[ivory|
    struct relay_struct
    { state         :: IBool
    ; defaultDelay  :: Uint32
    ; delay         :: Uint32
    ; timestamp     :: Uint32
    ; group         :: Uint8
    }
|]



data Relays = Relays
    { runRelays :: RunRecords RelayStruct
    , payload   :: Buffer 8 Uint8
    }

relays :: String -> Int -> Relays
relays name n = Relays
    { runRelays = runRecords name $ take n $ go <$> iterate (+1) 1
    , payload   = buffer "relay_message"
    } where go i = [ state        .= ival false
                   , defaultDelay .= ival 0
                   , delay        .= ival 0
                   , timestamp    .= ival 0
                   , group        .= ival i
                   ]




message :: Relays -> Uint8 -> Ivory eff (Buffer 8 Uint8)
message (Relays runRelay payload) i = do
    let payload' = addrOf payload
    runRelay $ \r -> do
        let relay = addrOf r ! toIx i
        pack   payload' 0 (0 :: Uint8)
        pack   payload' 1 $ i + 1
        pack   payload' 2 =<< deref (relay ~> state)
        pack   payload' 3 =<< deref (relay ~> group)
        packLE payload' 4 =<< deref (relay ~> defaultDelay)
    pure payload




getState :: Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff IBool
getState = get state

getDefaultDelay :: Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff Uint32
getDefaultDelay = get defaultDelay

getDelay :: Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff Uint32
getDelay = get delay

getTimestamp :: Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff Uint32
getTimestamp = get timestamp

getGroup :: Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff Uint8
getGroup = get group



setState :: IBool -> Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
setState = set state

setDefaultDelay :: Uint32 -> Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
setDefaultDelay = set defaultDelay

setDelay :: Uint32 -> Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
setDelay = set delay

setGroup :: Uint8 -> Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
setGroup = set group

setTimestamp :: Uint32 -> Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
setTimestamp = set timestamp



turnOff :: Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
turnOff = setState false

turnOn :: Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
turnOn i rs = do
    delay <- getDefaultDelay i rs
    turnOnDelayed delay i rs

turnOnDelayed :: Uint32 -> Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
turnOnDelayed delay i rs = do
    setDelay  delay i rs
    setState  true  i rs


get :: IvoryStore t
    => Label RelayStruct (Stored t)
    -> Relays
    -> (forall n. KnownNat n => Ix n)
    -> Ivory eff t
get f rs i = runRelays rs $ \r -> deref (addrOf r ! i ~> f)

set :: IvoryStore t
    => Label RelayStruct (Stored t)
    -> t
    -> Relays
    -> (forall n. KnownNat n => Ix n)
    -> Ivory eff ()
set f v rs i = runRelays rs $ \r -> store (addrOf r ! i ~> f) v



instance KnownNat n => Include (Records n RelayStruct) where
    include r = do
        defStruct (Proxy :: Proxy RelayStruct)
        defMemArea r

instance Include Relays where
    include (Relays {runRelays, payload}) =
        runRelays include >> include payload
