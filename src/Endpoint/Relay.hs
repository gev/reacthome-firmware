{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

module Endpoint.Relay where

import           Data.Function
import           Include
import           Initialize
import           Interface.GPIO
import           Interface.Timer
import           Ivory.Language
import           Util.Data.Class
import           Util.Data.Value


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

apply :: Relay -> Ivory eff ()
apply (Relay out state) = do
    s <- getValue state
    ifte_ s (set   out) 
            (reset out)



instance Include Relay where
    include (Relay {out, state}) = include state >> include out

instance Initialize Relay where
    initialize (Relay {out}) = initialize out
