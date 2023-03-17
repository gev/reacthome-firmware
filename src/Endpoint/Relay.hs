{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Endpoint.Relay where

import           Control.Monad.Writer
import           Core.Context
import           Data.Buffer
import           Data.Value
import           Feature.Relays        (initRelays)
import           Interface.GPIO.Output
import           Ivory.Language


data Relay = forall o. Output o => Relay
    { n       :: Int
    , name    :: String
    , out     :: o
    , state   :: Value IBool
    , payload :: Buffer 8 Uint8
    }


relay :: (Monad m, Output o) => Int -> o -> WriterT Context m Relay
relay n out = do
    let name  = "relay_" <> show n

    state    <- value  (name <> "_state"  ) false
    payload  <- buffer (name <> "_payload")

    let initRelay' :: Def ('[] ':-> ())
        initRelay' = proc (name <> "_payload_init") $ body $ do
            let payload' = addrOf payload
            store (payload' ! 0) 0
            store (payload' ! 1) $ fromIntegral n
            store (payload' ! 2) 0
            store (payload' ! 3) $ fromIntegral n
            store (payload' ! 4) 0
            store (payload' ! 5) 0
            store (payload' ! 6) 0
            store (payload' ! 7) 0
    include initRelay'

    pure Relay { n, name , out, state, payload }



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
