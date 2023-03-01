{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Feature.Relays where

import           Control.Monad.Reader  (Reader, asks)
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Include
import           Core.Initialize
import           Core.Task
import qualified Core.Transport        as T
import           Data.Buffer
import           Data.Foldable
import           Data.Value
import           Endpoint.Relay
import           GHC.TypeNats
import           Interface.GPIO.Output
import           Interface.MCU         (MCU)
import           Ivory.Language
import           Ivory.Stdlib

data Relays = Relays
    { getRelays :: [Relay]
    , transmit  :: forall s n. (KnownNat n) => Buffer n Uint8 -> Ivory (ProcEffects s ()) ()
    }

relays :: (MCU mcu, Output o, T.Transport t)
       => [mcu -> o] -> Reader (Domain mcu t) Feature
relays outs = do
    mcu       <- asks mcu
    transport <- asks transport
    let os = ($ mcu) <$> outs
    pure . Feature $ Relays { getRelays = zipWith relay [1..] os
                            , transmit  = T.transmit transport
                            }


instance Include Relays where
    include = traverse_ include . getRelays

instance Initialize Relays where
    initialize = concatMap initialize . getRelays

instance Task Relays where
    tasks (Relays {getRelays}) = [
            delay 10 "relays" $ traverse_ manage getRelays
        ]


instance Controller Relays where
    handle (Relays rs transmit) buff size = do
        action <- deref $ addrOf buff ! 0
        index  <- deref $ addrOf buff ! 1
        state  <- deref $ addrOf buff ! 2
        let go f r i = index ==? fromIntegral i ==> f r >> transmit (payload r)
        let run f = cond_ (zipWith (go f) rs [1..])
        pure [ action ==? 0 ==> cond_ [ state ==? 1 ==> run turnOn
                                      , state ==? 0 ==> run turnOff
                                      ]
             ]
