{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
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
    { state     :: IBool
    ; delay     :: Uint32
    ; timestamp :: Uint32
    }
|]



data Relays = Relays
    { runRelays :: RunRecords RelayStruct
    , payload   :: Buffer 8 Uint8
    }

relays :: String -> Int -> Relays
relays name n = Relays
    { runRelays = runRecords name ( replicate n [ state     .= ival false
                                                , delay     .= ival 0
                                                , timestamp .= ival 0
                                                ]
                                  )
    , payload   = buffer "relay_message"
    }





message :: Relays -> Uint8 -> Ivory eff (Buffer 8 Uint8)
message (Relays runRelay payload) i = do
    let payload' = addrOf payload
    runRelay $ \r -> do
        let relay = addrOf r ! toIx i
        packBE payload' 0 (0 :: Uint8)
        packBE payload' 1 $ i + 1
        packBE payload' 2 =<< deref (relay ~> state)
        packBE payload' 3 $ i + 1
        packBE payload' 4 (0 :: Uint32)
    pure payload




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
    include (Relays {runRelays, payload}) =
        runRelays include >> include payload
