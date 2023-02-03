{-# LANGUAGE GADTs #-}

module Feature.Relays where

import           Data.Foldable
import           Endpoint.Relay
import           Feature
import           Include
import           Initialize
import           Interface.GPIO
import           Ivory.Language
import           Util.Data.Value
-- data Relay = forall a. (OUT a) => Relay Int a

-- relay :: OUT a => Int -> a -> Feature
-- relay n = Feature . Relay n

newtype Relays = Relays
    { getRelays :: [Relay]
    }

relays :: [Relay] -> Feature
relays rs = Feature $ Relays
    { getRelays = rs
    }

instance Include Relays where
    include = traverse_ include . getRelays

instance Initialize Relays where
    initialize = concatMap initialize . getRelays

instance Task Relays where
    tasks (Relays rs) = [
            delay 10 "relays" $ traverse_ apply rs
        ]
