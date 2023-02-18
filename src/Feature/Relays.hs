{-# LANGUAGE GADTs #-}

module Feature.Relays where

import           Control.Monad.Reader
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Include
import           Core.Initialize
import           Core.Task
import           Data.Foldable
import           Data.Value
import           Endpoint.Relay
import           Interface.GPIO
import           Interface.MCU        (MCU)
import           Ivory.Language


newtype Relays = Relays
    { getRelays :: [Relay]
    }

relays :: (MCU mcu, OUT o) => [mcu -> o] -> Reader (Domain mcu) Feature
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
            delay 10 "relays" $ traverse_ apply rs
        ]

instance Controller Relays
