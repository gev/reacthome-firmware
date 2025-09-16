{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE TypeApplications    #-}

module Feature.Touches where

import           Control.Monad                 (replicateM, replicateM_)
import           Control.Monad.Reader          (MonadReader, asks)
import           Control.Monad.State           (MonadState (get))
import           Core.Actions
import           Core.Context
import qualified Core.Domain                   as D
import           Core.Handler                  (addHandler)
import           Core.Task
import qualified Core.Transport                as T
import           Data.Buffer
import           Data.Data
import           Data.Fixed                    as F
import           Data.Index
import           Data.Record
import           Data.Serialize
import           Data.Value
import qualified Endpoint.DInputs              as DI
import           Foreign                       (new)
import           GHC.Arr                       (array)
import           GHC.TypeNats
import           Interface.MCU                 (MCU, peripherals, systemClock)
import           Interface.SystemClock         (SystemClock)
import           Interface.Timer
import           Interface.Touch
import qualified Interface.Touch               as I
import           Ivory.Language
import           Ivory.Language.Proxy
import           Ivory.Stdlib
import           Support.Device.GD32F3x0.Timer (readCounter, timer14)



data Touches n = forall to. (I.Touch to) => Touches
    { getTouches    :: List n to
    , getDInputs    :: DI.DInputs n
    , indexTouch    :: Value Uint8
    , buf           :: Buffer 14 Uint8
    , transmit      :: forall l. KnownNat l
                    => Buffer l Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }


touches :: forall m n p c to t tr.
           ( MonadState Context m
           , MonadReader (D.Domain p c) m
           , T.Transport tr
           , I.Touch to
           , KnownNat n
           )
        => IFloat -> IFloat -> List n (p -> IFloat -> IFloat -> m to) -> tr -> m (Touches n)
touches thresholdLow thresholdHigh touches' transport = do
    mcu            <- asks D.mcu
    ts             <- traverse (\touch -> touch (peripherals mcu) thresholdLow thresholdHigh) touches'
    indexTouch     <- index "index_touches"
    dinputs        <- DI.mkDinputs "touches"
    buf            <- buffer "touch_buffer"

    let touches = Touches { getTouches = ts
                          , getDInputs = dinputs
                          , indexTouch
                          , buf
                          , transmit = T.transmitBuffer transport
                          }

    addTask  $ delay 50  "touches_log"    $ sendTimeTask   touches
    -- addTask  $ yeld      "touches_run"    $ touchesRunTask touches
    -- addTask  $ yeld      "touches_run"  (I.run . F.head . F.tail . F.tail $ ts)

    overSingleTouch touches $ \t i -> do
        addTask (delay 1 ("touches_run_" <> show i) $ replicateM_ 10 (I.run t))

    addTask  $ delay 10  "touches_manage" $ manageTouches  touches
    addTask  $ yeld      "touches_sync"   $ syncTouches    touches

    addSync "touches" $ forceSyncTouches touches

    pure touches

sendTimeTask :: KnownNat n => Touches n -> Ivory (ProcEffects s ()) ()
sendTimeTask touches@Touches{..} = do
    let n = length getTouches
    overSingleTouch touches \t i -> do
        time <- I.getDebug t
        packBE buf (fromIntegral $ i * 2 + 2) $ castDefault @Sint16 time
    store (buf ! 0) actionError
    store (buf ! 1) 1 -- type debug message


    transmit buf


touchesRunTask :: KnownNat n => Touches n -> Ivory (ProcEffects s ()) ()
touchesRunTask touches@Touches{..} = do
    -- mapM_ I.run getTouches
    -- mapM_ I.reset getTouches

    I.run . F.head . F.tail . F.tail $ getTouches


overSingleTouch :: (KnownNat n, Monad m) => Touches n -> (forall to. I.Touch to => to -> Integer -> m ()) -> m ()
overSingleTouch Touches{..} handle =
    zipWithM_ handle getTouches ints



forceSyncTouches :: KnownNat n => Touches n -> Ivory eff ()
forceSyncTouches Touches{..} =
    arrayMap $ \ix -> store (( DI.dinputs getDInputs ! ix) ~> DI.synced) false



manageTouches :: KnownNat n => Touches n -> Ivory eff ()
manageTouches Touches{..} =
    zipWithM_ zip getTouches ints
    where
        zip :: I.Touch i => i -> Int -> Ivory eff ()
        zip touch i = do
            let ix = fromIntegral i
            let dt = DI.dinputs getDInputs ! ix
            manageTouch dt touch



manageTouch :: I.Touch i
             => Record DI.DInputStruct
             -> i
             -> Ivory eff ()
manageTouch di touch = do
    state0 <- deref $ di ~> DI.state
    state1 <- getState touch
    when (state1 /=? state0) $ do
        store (di ~> DI.state    ) state1
        store (di ~> DI.synced   ) false



syncTouches :: KnownNat n => Touches n -> Ivory (ProcEffects s ()) ()
syncTouches touch@Touches{..} = do
    i <- deref indexTouch
    syncTouch touch i
    store indexTouch $ i + 1



syncTouch :: KnownNat n => Touches n -> Uint8 -> Ivory (ProcEffects s ()) ()
syncTouch Touches{..} i = do
    let n = fromIntegral $ length getTouches
    let di = DI.dinputs getDInputs ! toIx i
    synced <- deref $ di ~> DI.synced
    when (iNot synced) $ do
        msg <- DI.message getDInputs (i .% n)
        transmit msg
        store (di ~> DI.synced) true
