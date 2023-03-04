{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Feature.Relay where

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
import qualified Endpoint.Relay        as E
import           GHC.TypeNats
import           Interface.GPIO.Output
import           Interface.MCU         (MCU)
import           Ivory.Language
import           Ivory.Stdlib

data Relay = Relay
    { getRelay :: E.Relay
    , transmit :: forall s n. (KnownNat n) => Buffer n Uint8 -> Ivory (ProcEffects s ()) ()
    }

relay :: (MCU mcu, Output o, T.Transport t)
       => (mcu -> o) -> Reader (Domain mcu t) Feature
relay out = do
    mcu       <- asks mcu
    transport <- asks transport
    let o = out mcu
    pure . Feature $ Relay { getRelay = E.relay 1 o
                           , transmit = T.transmit transport
                           }


instance Include Relay where
    include = include . getRelay

instance Initialize Relay where
    initialize = initialize . getRelay

instance Task Relay where
    tasks (Relay {getRelay}) = [
            delay 10 "relays" $ E.manage getRelay
        ]


instance Controller Relay where
    handle (Relay r transmit) buff size = do
        let buff' = addrOf buff
        action <- deref $ buff' ! 0
        index  <- deref $ buff' ! 1
        state  <- deref $ buff' ! 2
        let run f = f r >> transmit (E.payload r)
        pure [ action ==? 0 ==> cond_ [ state ==? 1 ==> run E.turnOn
                                      , state ==? 0 ==> run E.turnOff
                                      ]
             ]