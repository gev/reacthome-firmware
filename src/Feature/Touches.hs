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

module Feature.Touches where

import           Control.Monad.Reader  (MonadReader, asks)
import           Control.Monad.State   (MonadState (get))
import           Core.Actions
import           Core.Context
import qualified Core.Domain           as D
import           Core.Handler          (addHandler)
import           Core.Task
import qualified Core.Transport        as T
import           Data.Buffer
import           Data.Data
import           Data.Fixed
import           Data.Index
import           Data.Record
import           Data.Serialize
import           Data.Value
import qualified Endpoint.DInputs     as DI
import           Foreign               (new)
import           GHC.Arr               (array)
import           GHC.TypeNats
import           Interface.MCU         (MCU, peripherals, systemClock)
import           Interface.SystemClock (SystemClock)
import           Interface.Timer
import           Interface.Touch
import qualified Interface.Touch       as I
import           Ivory.Language
import           Ivory.Language.Proxy
import           Ivory.Stdlib



data Touches n = forall to. (I.Touch to) => Touches
    { getTouches    :: List n to
    , getDInputs    :: DI.DInputs n
    , currentTouch  :: Value (Ix n)
    , indexTouch    :: Value Uint8
    , buf           :: Buffer n Uint8
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
        => Uint16 -> Uint16 -> List n (p -> Uint16 -> Uint16 -> m to) -> tr -> m (Touches n)
touches tresholdLower tresholdUpper touches' transport  = do
    mcu            <- asks D.mcu
    ts             <- traverse (\touch -> touch (peripherals mcu) tresholdLower tresholdUpper) touches' 
    currentTouch   <- index "current_touches"
    indexTouch     <- index "index_touches"
    dinputs        <- DI.mkDinputs "touches"
    buf            <- buffer "touch_buffer"


    let touches = Touches { getTouches = ts
                          , getDInputs = dinputs
                          , currentTouch
                          , indexTouch
                          , buf
                          , transmit = T.transmitBuffer transport
                          }


    -- addTask $ delay 50 "sync"    $ sendTimeTask touches
    addTask  $ yeld     "touches_run"    $ touchesRunTask touches
    addTask  $ delay 10 "touches_manage" $ manageTouches touches
    addTask  $ yeld     "touches_sync"   $ syncTouches   touches
    addSync "touches" $ forceSyncTouches touches

    pure touches

sendTimeTask :: KnownNat n => Touches n -> Ivory (ProcEffects s ()) ()
sendTimeTask touches@Touches{..} = do
    let n = length getTouches
    arrayMap $ \ix ->
        overSingleTouch touches ix \t -> do
            -- time <- (128 +) <$> I.getTime t
            -- cond_ [ time <? 0 ==> store (buf ! ix) 0
            --       , time >? 255 ==> store (buf ! ix) 255
            --       , true ==> store (buf ! ix) (castDefault time)
            --       ]

            time <- I.getTime t
            ifte_ (time <? 255)
                (store (buf ! ix) $ castDefault time)
                (store (buf ! ix) 255)


            -- state <- I.getStateBtn t
            -- ifte_ state
            --     (store (buf ! ix) 100)
            --     (store (buf ! ix) 0)

    store (buf ! 0) actionDoppler1

    transmit buf


touchesRunTask :: KnownNat n => Touches n -> Ivory eff ()
touchesRunTask touches@Touches{..} = do
    cur <- deref currentTouch
    overSingleTouch touches cur \t -> I.run t (store currentTouch $ cur + 1)



overSingleTouch :: KnownNat n => Touches n -> Ix n -> (forall to. I.Touch to => to -> Ivory eff ()) -> Ivory eff ()
overSingleTouch Touches{..} current handle = zipWithM_ run getTouches ints
        where run touch i = do
                let ix = fromIntegral i
                when (ix ==? current) $ do
                    handle touch



forceSyncTouches :: KnownNat n => Touches n -> Ivory eff ()
forceSyncTouches Touches{..} = do
    arrayMap $ \ix -> store (( DI.dinputs getDInputs ! ix) ~> DI.synced) false



manageTouches :: KnownNat n => Touches n -> Ivory eff ()
manageTouches Touches{..} = zipWithM_ zip getTouches ints
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
