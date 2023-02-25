{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}

module Endpoint.Relay where

import           Core.Include
import           Core.Initialize
import           Data.Buffer
import           Data.Function
import           Data.Value
import           Interface.GPIO
import           Interface.Timer
import           Ivory.Language


-- [ivory|
--     struct Relay
--     {

--     }
-- |]



data Relay = forall a. (Out a) => Relay
    { n       :: Int
    , name    :: String
    , out     :: a
    , state   :: Value IBool
    , payload :: Buffer 8 Uint8
    }


relay :: Out o => Int -> o -> Relay
relay n out = Relay
    { n       = n
    , name    = name
    , out     = out
    , state   = value (name <> "_state") false
    , payload = buffer (name <> "_payload")
    } where name = "relay_" <> show n



turnOn :: Relay -> Ivory eff ()
turnOn (Relay {state, payload}) =
    store (addrOf state) true >> store (addrOf payload ! 2) 1

turnOff :: Relay -> Ivory eff ()
turnOff (Relay {state, payload}) = do
    store (addrOf state) false >> store (addrOf payload ! 2) 0

manage :: Relay -> Ivory eff ()
manage (Relay {out, state}) = do
    s <- deref $ addrOf state
    ifte_ s (set   out)
            (reset out)



instance Include Relay where
    include (Relay {out, state, payload}) = do
        include out
        include state
        include payload

instance Initialize Relay where
    initialize (Relay {n, name, out, payload}) =
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
