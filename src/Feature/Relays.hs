{-# LANGUAGE GADTs #-}

module Feature.Relays where

import           Data.Foldable
import           Endpoint.Relay
import           Feature
import           Include
import           Initialize
import           Interface.GPIO
import           Interface.MCU   (MCU)
import           Ivory.Language
import           Util.Data.Value


newtype Relays = Relays
    { getRelays :: [Relay]
    }

relays :: (MCU mcu, OUT o) => [mcu -> o] -> mcu -> Feature
relays os mcu = Feature $ Relays
    { getRelays = zipWith relay [1..] $ ($ mcu) <$> os
    }


instance Include Relays where
    include = traverse_ include . getRelays

instance Initialize Relays where
    initialize = concatMap initialize . getRelays

instance Task Relays where
    tasks (Relays rs) = [
            delay 10 "relays" $ traverse_ apply rs
        ]
