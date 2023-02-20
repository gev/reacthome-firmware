{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Endpoint.Relay where

import           Core.Include
import           Core.Initialize
import           Data.Buffer
import           Data.Class
import           Data.Function
import           Data.Value
import           Interface.GPIO
import           Interface.Timer
import           Ivory.Language


data Relay = forall a. (OUT a) => Relay
    { n       :: Int
    , name    :: String
    , out     :: a
    , state   :: Value IBool
    , payload :: Buffer 8 Uint8
    }


relay :: OUT o => Int -> o -> Relay
relay n out = Relay
    { n       = n
    , name    = name
    , out     = out
    , state   = value (name <> "_state") false
    , payload = buffer (name <> "_payload")
    } where name = "relay_" <> show n



turnOn :: Relay -> Ivory eff ()
turnOn (Relay {state, payload}) =
    setValue state true >> setItem payload 2 1

turnOff :: Relay -> Ivory eff ()
turnOff (Relay {state, payload}) = do
    setValue state false >> setItem payload 2 0

manage :: Relay -> Ivory eff ()
manage (Relay {out, state}) = do
    s <- getValue state
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
                setItem payload 0 0
                setItem payload 1 $ fromIntegral n
                setItem payload 2 0
                setItem payload 3 $ fromIntegral n
                setItem payload 4 0
                setItem payload 5 0
                setItem payload 6 0
                setItem payload 7 0
        ]
