{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoint.Relay where

import           Core.Include
import           Core.Initialize
import           Data.Buffer
import           Data.Value
import           Interface.GPIO.Output
import           Ivory.Language


data Relay = forall o. Output o => Relay
    { n       :: Int
    , name    :: String
    , out     :: o
    , state   :: Value IBool
    , payload :: Buffer 8 Uint8
    }


relay :: Output o => Int -> o -> Relay
relay n out = Relay
    { n       = n
    , name    = name
    , out     = out
    , state   = value (name <> "_state") false
    , payload = buffer (name <> "_payload")
    } where name = "relay_" <> show n



turnOn :: Relay -> Ivory eff ()
turnOn (Relay {..}) =
    store (addrOf state) true >> store (addrOf payload ! 2) 1

turnOff :: Relay -> Ivory eff ()
turnOff (Relay {..}) = do
    store (addrOf state) false >> store (addrOf payload ! 2) 0

manage :: Relay -> Ivory eff ()
manage (Relay {..}) = do
    s <- deref $ addrOf state
    ifte_ s (set   out)
            (reset out)



instance Include Relay where
    include (Relay {..}) = do
        include out
        include state
        include payload

instance Initialize Relay where
    initialize (Relay {..}) =
        initialize out <> [
            proc (name <> "_payload_init") $ body $ do
                let payload' = addrOf payload
                store (payload' ! 0) 0
                store (payload' ! 1) $ fromIntegral n
                store (payload' ! 2) 0
                store (payload' ! 3) $ fromIntegral n
                store (payload' ! 4) 0
                store (payload' ! 5) 0
                store (payload' ! 6) 0
                store (payload' ! 7) 0
        ]
