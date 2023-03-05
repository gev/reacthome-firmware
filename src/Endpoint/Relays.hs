{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

module Endpoint.Relays where

import           Core.Include
import qualified Core.Transport as T
import           Data.Buffer
import           Data.Record
import           Data.Serialize
import           Endpoint.Group (setTimestamp)
import           Endpoint.Relay (Relay (payload))
import           GHC.TypeNats
import           Interface.MCU
import           Ivory.Language



type RelayStruct = "relay_struct"

[ivory|
    struct relay_struct
    { state     :: IBool
    ; delay     :: Uint32
    ; timestamp :: Uint32
    }
|]



data Relays = forall t. T.Transport t => Relays
    { runRelays :: RunRecords RelayStruct
    , transport :: t
    , message   :: Buffer 8 Uint8
    }

relays :: (T.Transport t) => String -> Int -> t -> Relays
relays name n transport = Relays
    { runRelays = runRecords name ( replicate n [ state     .= ival false
                                                , delay     .= ival 0
                                                , timestamp .= ival 0
                                                ]
                                  )
    , transport = transport
    , message   = buffer "relay_message"
    }




transmit :: Relays -> Uint8 -> Ivory (ProcEffects s ()) ()
transmit (Relays runRelay transport message) i =
    runRelay $ \r -> do
        let relay = addrOf r ! toIx i
        let message' = addrOf message
        packBE message' 0 (0 :: Uint8)
        packBE message' 1 $ i + 1
        packBE message' 2 =<< deref (relay ~> state)
        packBE message' 3 $ i + 1
        packBE message' 4 (0 :: Uint32)
        T.transmit transport message



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
    include (Relays {runRelays, message}) =
        runRelays include >> include message
