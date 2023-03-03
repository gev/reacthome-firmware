{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.Relays where

import           Control.Monad.Reader    (Reader, asks)
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Include
import           Core.Initialize
import           Core.Task
import qualified Core.Transport          as T
import           Data.Buffer
import           Data.Value
import qualified Endpoint.Relays         as E
import           GHC.TypeNats
import           Interface.GPIO.Output
import           Interface.GPIOs.Outputs as I
import           Interface.MCU           (MCU)
import           Ivory.Language
import           Ivory.Language.Array
import           Ivory.Stdlib



data Relays = forall os. Outputs os => Relays
    { getRelays  :: E.Relays
    , getOutputs :: os
    , message    :: Buffer 8 Uint8
    , transmit   :: Buffer 8 Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }



relays :: (T.Transport t, MakeOutputs o os)
       => [mcu -> o] -> Reader (Domain mcu t) Feature
relays outs = do
    mcu       <- asks mcu
    transport <- asks transport
    let os = ($ mcu) <$> outs
    let n = length os
    pure . Feature $ Relays { getRelays  = E.relays "relays" n
                            , getOutputs = makeOutputs "relays_outputs" os
                            , message    = values "relay_message" [0, 0, 0, 0, 0, 0, 0, 0]
                            , transmit   = T.transmit transport
                            }



instance Include Relays where
    include (Relays {getRelays, getOutputs, message}) = do
        include getRelays
        include getOutputs
        include message



instance Initialize Relays where
    initialize (Relays {getOutputs}) =
        initialize getOutputs



instance Task Relays where
    tasks rs = [
            delay 10 "relays" $ manage rs
        ]

manage :: Relays -> Ivory eff ()
manage (Relays {getRelays, getOutputs}) = do
    E.runRelays getRelays $ \rs -> do
        let rs' = addrOf rs
        arrayMap $ \ix -> do
            isOn <- deref $ rs' ! ix ~> E.state
            ifte_ isOn
                  (I.set getOutputs (toIx $ fromIx ix))
                  (I.reset getOutputs (toIx $ fromIx ix))



instance Controller Relays where
    handle rs buff size = do
        let buff' = addrOf buff
        action <- deref $ buff' ! 0
        index  <- deref $ buff' ! 1
        state  <- deref $ buff' ! 2
        pure [ action ==? 0 ==> cond_ [ state ==? 1 ==> turnOn  rs index
                                      , state ==? 0 ==> turnOff rs index
                                      ]
             ]

turnOn :: Relays -> Uint8 -> Ivory (ProcEffects s ()) ()
turnOn (Relays {getRelays, transmit, message}) i = do
    let message' = addrOf message
    E.turnOn getRelays $ toIx i
    store (message' ! 1) i
    store (message' ! 2) 1
    store (message' ! 3) i
    transmit message

turnOff :: Relays -> Uint8 -> Ivory (ProcEffects s ()) ()
turnOff (Relays {getRelays, transmit, message}) i = do
    let message' = addrOf message
    E.turnOff getRelays $ toIx i
    store (message' ! 1) i
    store (message' ! 2) 0
    store (message' ! 3) i
    transmit message
