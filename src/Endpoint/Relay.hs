{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Endpoint.Relay where

import           Core.Include
import           Core.Initialize
import           Data.Class
import           Data.Function
import           Data.Value
import           Interface.GPIO
import           Interface.Timer
import           Ivory.Language


data Relay = forall a. (OUT a) => Relay
 { out   :: a
 , state :: Value IBool
 }


relay :: OUT o => Int -> o -> Relay
relay n out = Relay
    { out   = out
    , state = value (name <> "_state") false
    } where name = "relay_" <> show n



turnOn :: Relay -> Ivory eff ()
turnOn (Relay {state}) = setValue state true

turnOff :: Relay -> Ivory eff ()
turnOff (Relay {state}) = setValue state false

manage :: Relay -> Ivory eff ()
manage (Relay out state) = do
    s <- getValue state
    ifte_ s (set   out)
            (reset out)



instance Include Relay where
    include (Relay {out, state}) = include state >> include out

instance Initialize Relay where
    initialize (Relay {out}) = initialize out
