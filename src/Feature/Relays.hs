{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Feature.Relays where

import           Control.Monad.Reader (Reader, asks)
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Include
import           Core.Initialize
import           Core.Task
import           Data.Buffer
import           Data.Class
import           Data.Foldable
import           Data.Value
import           Endpoint.Relay
import           GHC.TypeNats
import           Interface.GPIO
import           Interface.MCU        (MCU)
import           Ivory.Language
import           Ivory.Stdlib


newtype Relays = Relays
    { getRelays :: [Relay]
    }

relays :: (MCU mcu, OUT o) => [mcu -> o] -> Reader (Domain mcu t) Feature
relays outs = do
    mcu <- asks mcu
    let os = ($ mcu) <$> outs
    pure . Feature $ Relays { getRelays = zipWith relay [1..] os
                            }


instance Include Relays where
    include = traverse_ include . getRelays

instance Initialize Relays where
    initialize = concatMap initialize . getRelays

instance Task Relays where
    tasks (Relays rs) = [
            delay 10 "relays" $ traverse_ manage rs
        ]


instance Controller Relays where
    handle (Relays rs) buff size = do
        action <- getItem buff 0
        index  <- getItem buff 1
        state  <- getItem buff 2
        let go f r i = index ==? fromIntegral i ==> f r
        let run f = cond_ (zipWith (go f) rs [1..])
        pure [ action ==? 1 ==> run turnOn
             , action ==? 0 ==> run turnOff
             ]
