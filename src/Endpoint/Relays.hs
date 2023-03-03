{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

module Endpoint.Relays where

import           Core.Include
import           Data.Record
import           Endpoint.Group (setTimestamp)
import           Endpoint.Relay (Relay (payload))
import           GHC.TypeNats
import           Ivory.Language



type RelayStruct = "relay_struct"

[ivory|
    struct relay_struct
    { state     :: IBool
    ; delay     :: Uint32
    ; timestamp :: Uint32
    }
|]



newtype Relays = Relays {runRelays :: RunRecords RelayStruct}

relays :: String -> Int -> Relays
relays name n = Relays $ runRecords name
                       ( replicate n [ state     .= ival false
                                     , delay     .= ival 0
                                     , timestamp .= ival 0
                                     ]
                       )

getState :: Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff IBool
getState = get state


setState :: IBool -> Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
setState = set state

setDelay :: Uint32 -> Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
setDelay = set delay

setTimestamp :: Uint32 -> Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
setTimestamp = set timestamp

turnOn :: Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
turnOn = setState true

turnOff :: Relays -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
turnOff = setState false



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
    include rs = runRelays rs include
