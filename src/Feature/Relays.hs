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
import           Core.Transport
import qualified Core.Transport          as T
import qualified Endpoint.Relays         as R
import           GHC.TypeNats
import           Interface.GPIO.Output
import           Interface.GPIOs.Outputs as I
import           Interface.MCU           (MCU)
import           Ivory.Language
import           Ivory.Language.Array
import           Ivory.Stdlib



data Relays = forall os. Outputs os => Relays
    { n          :: Uint8
    , getRelays  :: R.Relays
    , getOutputs :: os
    }



relays :: (Transport t, MakeOutputs o os)
       => [mcu -> o] -> Reader (Domain mcu t) Feature
relays outs = do
    mcu       <- asks mcu
    transport <- asks transport
    let os = ($ mcu) <$> outs
    let n = length os
    pure . Feature $ Relays { n = fromIntegral n
                            , getRelays  = R.relays "relays" n transport
                            , getOutputs = makeOutputs "relays_outputs" os
                            }



instance Include Relays where
    include (Relays {getRelays, getOutputs}) = do
        include getRelays
        include getOutputs



instance Initialize Relays where
    initialize (Relays {getOutputs}) =
        initialize getOutputs



instance Task Relays where
    tasks rs = [
            delay 10 "relays" $ manage rs
        ]

manage :: Relays -> Ivory eff ()
manage (Relays {getRelays, getOutputs}) = do
    R.runRelays getRelays $ \rs -> do
        let rs' = addrOf rs
        arrayMap $ \ix -> do
            isOn <- deref $ rs' ! ix ~> R.state
            ifte_ isOn
                  (I.set getOutputs (toIx $ fromIx ix))
                  (I.reset getOutputs (toIx $ fromIx ix))



instance Controller Relays where
    handle rs buff size = do
        let buff' = addrOf buff
        pure [ size >=? 3 ==> do
                action <- deref $ buff' ! 0
                cond_ [action ==? 0 ==> onDo rs buff' size
                      ]
             ]

onDo :: KnownNat l
     => Relays
     -> Ref Global ('Array l ('Stored Uint8))
     -> n
     -> Ivory (ProcEffects s ()) ()
onDo rs buff size = do
    index  <- deref $ buff ! 1
    action <- deref $ buff ! 2
    cond_ [ action ==? 0 ==> run R.turnOff rs index
          , action ==? 1 ==> run R.turnOn  rs index
          , action ==? 2 ==> pure ()
          , action ==? 3 ==> pure ()
          ]



run :: (R.Relays -> (forall n. KnownNat n => Ix n) -> Ivory (ProcEffects s ()) a)
    -> Relays
    -> Uint8
    -> Ivory (ProcEffects s ()) ()
run action (Relays {n, getRelays}) i = do
    when (i >=? 1 .&& i <=? n) $ do
        let i' = i - 1
        action getRelays $ toIx i'
        R.transmit getRelays i'
