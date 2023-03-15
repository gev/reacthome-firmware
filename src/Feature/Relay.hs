{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Relay where

import           Control.Monad.Reader  (Reader, asks)
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Task
import qualified Core.Transport        as T
import           Data.Buffer
import           Data.Value
import qualified Endpoint.Relay        as E
import           GHC.TypeNats
import           Interface.GPIO.Output
import           Interface.MCU         (MCU, peripherals)
import           Ivory.Language
import           Ivory.Stdlib

data Relay = Relay
    { getRelay :: E.Relay
    , transmit :: forall s n. (KnownNat n) => Buffer n Uint8 -> Ivory (ProcEffects s ()) ()
    }

relay :: (Output o, T.Transport t)
       => (p -> o) -> Reader (Domain p t) Feature
relay out = do
    mcu       <- asks mcu
    transport <- asks transport
    let o = out $ peripherals mcu
    pure . Feature $ Relay { getRelay = E.relay 1 o
                           , transmit = T.transmit transport
                           }


instance Include Relay where
    include = include . getRelay

instance Task Relay where
    tasks (Relay {..}) = [
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
