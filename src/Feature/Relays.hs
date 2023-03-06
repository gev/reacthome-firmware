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
import qualified Core.Domain             as D
import           Core.Feature
import           Core.Include
import           Core.Initialize
import           Core.Task
import qualified Core.Transport          as T
import           Data.Buffer
import           Data.Serialize
import           Data.Value
import qualified Endpoint.Groups         as G
import qualified Endpoint.Relays         as R
import           GHC.TypeNats
import           Interface.GPIO.Output
import           Interface.GPIOs.Outputs as I
import           Interface.MCU           (MCU)
import           Ivory.Language
import           Ivory.Stdlib



data Relays = forall os. Outputs os => Relays
    { n          :: Uint8
    , getRelays  :: R.Relays
    , getGroups  :: G.Groups
    , getOutputs :: os
    , shouldInit :: Value IBool
    , transmit   :: forall n. KnownNat n
                 => Buffer n Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }



relays :: (MakeOutputs o os, T.Transport t)
       => [mcu -> o] -> Reader (D.Domain mcu t) Feature
relays outs = do
    mcu        <- asks D.mcu
    transport  <- asks D.transport
    shouldInit <- asks D.shouldInit
    let os = ($ mcu) <$> outs
    let n = length os
    pure . Feature $ Relays { n = fromIntegral n
                            , getRelays  = R.relays "relays" n
                            , getGroups  = G.groups "groups" n
                            , getOutputs = makeOutputs "relays_outputs" os
                            , shouldInit = shouldInit
                            , transmit   = T.transmit transport
                            }



instance Include Relays where
    include (Relays {getRelays, getGroups, getOutputs}) = do
        include getOutputs
        include getRelays
        include getGroups



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
        shouldInit' <- deref . addrOf $ shouldInit rs
        pure [ size >=? 3 ==> do
                action <- deref $ buff' ! 0
                cond_ [ action ==? 0 .&& iNot shouldInit'
                            ==> onDo rs buff' size

                      , action ==? 2 .&& iNot shouldInit'
                            ==> onGroup rs buff' size

                      , action ==? 0xf2
                            ==> onInit rs buff' size
                      ]
             ]



onDo :: KnownNat l
     => Relays
     -> Ref Global ('Array l ('Stored Uint8))
     -> Uint8
     -> Ivory (ProcEffects s ()) ()
onDo rs buff size = do
    index  <- deref $ buff ! 1
    action <- deref $ buff ! 2
    cond_ [ action ==? 0
                ==> runRelays R.turnOff rs index

          , action ==? 1
                ==> ifte_
                    (size >=? 7)
                    (do delay <- unpackLE buff 3
                        runRelays (R.turnOnDelayed delay) rs index
                    )
                    (runRelays R.turnOn rs index)

          , action ==? 2 .&& size >=? 7
                ==> do
                    delay <- unpackLE buff 3
                    runRelays (R.setDefaultDelay delay) rs index

          , action ==? 3
                ==> do
                    group <- unpack buff 3
                    runRelays (R.setGroup group) rs index
          ]



runRelays :: (R.Relays -> (forall n. KnownNat n => Ix n) -> Ivory (ProcEffects s ()) a)
    -> Relays
    -> Uint8
    -> Ivory (ProcEffects s ()) ()
runRelays runAction (Relays {n, getRelays, transmit}) i = do
    when (i >=? 1 .&& i <=? n) $ do
        let i' = i - 1
        runAction getRelays (toIx i')
        transmit =<< R.message getRelays i'



onGroup :: KnownNat l
        => Relays
        -> Ref Global ('Array l ('Stored Uint8))
        -> Uint8
        -> Ivory (ProcEffects s ()) ()
onGroup rs buff size =
    when (size >=? 7) $ do
        index   <- unpack   buff 1
        enabled <- unpack   buff 2
        delay   <- unpackLE buff 3
        runGroups (G.setState enabled delay) rs index




runGroups :: (G.Groups -> (forall n. KnownNat n => Ix n) -> Ivory (ProcEffects s ()) a)
    -> Relays
    -> Uint8
    -> Ivory (ProcEffects s ()) ()
runGroups runAction (Relays {n, getGroups, transmit}) i = do
    when (i >=? 1 .&& i <=? n) $ do
        let i' = i - 1
        runAction getGroups (toIx i')
        transmit =<< G.message getGroups i'

{-
    TODO: Generalize initialization
-}
onInit :: KnownNat l
        => Relays
        -> Ref Global ('Array l ('Stored Uint8))
        -> Uint8
        -> Ivory (ProcEffects s ()) ()
onInit (Relays {n, getRelays, getGroups, shouldInit}) buff size =
    when (size >=? 5 * n + 6 * n) $ do
        j <- local $ ival 1
        G.runGroups getGroups $ \gs -> do
            let gs' = addrOf gs
            arrayMap $ \ix -> do
                j' <- deref j
                store (gs' ! ix ~> G.enabled) =<< unpack buff  j'
                store (gs' ! ix ~> G.delay) =<< unpackLE buff (j' + 1)
                store j $ j' + 5
        j' <- deref j
        R.runRelays getRelays $ \rs -> do
            let state = addrOf rs
            arrayMap $ \ix -> do
                j' <- deref j
                store (state ! ix ~> R.state) =<< unpack   buff  j'
                store (state ! ix ~> R.group) =<< unpack   buff (j' + 1)
                store (state ! ix ~> R.delay) =<< unpackLE buff (j' + 2)
                store j $ j' + 6
        store (addrOf shouldInit) false
