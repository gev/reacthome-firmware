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
import qualified Endpoint.DTouches     as DT
import           GHC.TypeNats
import           Interface.MCU         (MCU, peripherals, systemClock)
import           Interface.SystemClock (SystemClock)
import           Interface.Timer
import qualified Interface.Touch as I
import           Ivory.Language
import           Ivory.Language.Proxy
import           Ivory.Stdlib
import Foreign (new)
import Core.Actions
import Interface.Touch


startMeasuring= 0
waitInterrupt = 1
isMeasured = 2

data Touches n = forall to ti. (Timer ti, I.Touch to) => Touches
    { getTouches    :: List n to
    , getDTouches   :: DT.DTouches n
    , currentTouch  :: Index Uint8
    , timestamp     :: Value Uint16
    , stateMasurent :: Value Uint8
    , counter       :: ti
    , buf           :: Buffer 6 Uint8
    , transmit      :: forall l. KnownNat l
                    => Buffer l Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }


touches :: forall m n p c to t tr.
           ( MonadState Context m
           , MonadReader (D.Domain p c) m
           , T.Transport tr
           , I.Touch to
           , Timer t
           , KnownNat n
           )
        => (p -> Uint32 -> Uint32 -> m t) -> List n (p -> t -> m to) -> tr -> m (Touches n)
touches counter' touches' transport = do
    mcu            <- asks D.mcu
    counter        <- counter' (peripherals mcu) 84_000_000 0xFFFF
    ts             <- traverse ($ peripherals mcu) touches'
    timestamp      <- value "touch_timestamp" 0
    stateMasurent  <- value "touch_ready" 0
    currentTouch   <- index "current_touches"
    dtouches       <- DT.mkDTouches "touches"
    buf            <- buffer "touch_buffer" 
    

    let touches = Touches { getTouches = ts
                          , getDTouches = dtouches
                          , currentTouch
                          , timestamp
                          , stateMasurent
                          , counter
                          , buf
                          , transmit = T.transmitBuffer transport
                          }
    mapM_  (\t -> addHandler $ I.HandleTouch t $ handleTouch touches) ts

    addTask $ yeld "touches_time" $ touchMeasurentTask touches
    addTask $ delay 10 "sync" $ syncTask touches
    -- addTask  $ delay 10 "touches_manage" $ manageDInputs touches
    -- addTask  $ yeld     "touches_sync"   $ syncDInputs   touches

    -- addSync "touches" $ forceSyncDInputs touches

    pure touches

syncTask :: KnownNat n => Touches n -> Ivory (ProcEffects s ()) ()
syncTask Touches{..} = do
    a0 <- deref (DT.dtouches getDTouches ! toIx (0::Uint8) ~> DT.time)
    a1 <- deref (DT.dtouches getDTouches ! toIx (1::Uint8) ~> DT.time)
    a2 <- deref (DT.dtouches getDTouches ! toIx (2::Uint8) ~> DT.time)
    a3 <- deref (DT.dtouches getDTouches ! toIx (3::Uint8) ~> DT.time)
    a4 <- deref (DT.dtouches getDTouches ! toIx (4::Uint8) ~> DT.time)

    store (buf ! 0) actionDoppler1
    store (buf ! 1) $ castDefault $ a0 / 32
    store (buf ! 2) $ castDefault $ a1 / 32
    store (buf ! 3) $ castDefault $ a2 / 32
    store (buf ! 4) $ castDefault $ a3 / 32
    store (buf ! 5) $ castDefault $ a4 / 32

    transmit buf


handleTouch :: KnownNat n => Touches n -> Ivory eff ()
handleTouch touch@Touches{..} = do
    current <- deref currentTouch
    t <- getCounter counter
    store (DT.dtouches getDTouches ! toIx current ~> DT.timestamp) $ castDefault t
    store stateMasurent isMeasured


touchMeasurentTask :: KnownNat n => Touches n -> Ivory eff ()
touchMeasurentTask touches@Touches{..}  = do

    stateMasurent' <- deref stateMasurent
    when (stateMasurent' ==? startMeasuring) do
        t   <- getCounter counter
        cur <- deref currentTouch
        store timestamp $ castDefault t               
        overSingleTouch touches cur I.setModeInput
        overSingleTouch touches cur I.enable
        store stateMasurent waitInterrupt
        
    when (stateMasurent' ==? isMeasured) do
        cur <- deref currentTouch
        t0 <- deref timestamp
        t1 <- deref (DT.dtouches getDTouches ! toIx cur ~> DT.timestamp)
        let time = t1 - t0

        overSingleTouch touches cur I.setModeOutput
        

        processingTouch touches time

        store currentTouch $ cur + 1
        store stateMasurent startMeasuring



processingTouch :: KnownNat n => Touches n -> Uint16 -> Ivory eff ()
processingTouch Touches{..} time = do
    cur <- deref currentTouch
    previousTime <- deref (DT.dtouches getDTouches ! toIx cur ~> DT.time)
    store (DT.dtouches getDTouches ! toIx cur ~> DT.time) $ average previousTime (safeCast time)

    -- newTime <- deref (DT.dtouches getDTouches ! toIx cur ~> DT.time)
    -- timeMax <- deref (DT.dtouches getDTouches ! toIx cur ~> DT.timeMax)
    -- timeMin <- deref (DT.dtouches getDTouches ! toIx cur ~> DT.timeMin)

    -- when (newTime >? timeMax) $ do
    --     store (DT.dtouches getDTouches ! toIx cur ~> DT.timeMax) newTime
    
    -- when (newTime <? timeMin) $ do
    --     store (DT.dtouches getDTouches ! toIx cur ~> DT.timeMin) newTime

    -- ifte_ ((timeMax + timeMin) / 2 <? newTime)
    --       (store (DT.dtouches getDTouches ! toIx cur ~> DT.state) true)
    --       (store (DT.dtouches getDTouches ! toIx cur ~> DT.state) false)
    



overSingleTouch :: KnownNat n => Touches n -> Uint8 -> (forall to. I.Touch to => to -> Ivory eff ()) -> Ivory eff ()
overSingleTouch Touches{..} current handle = zipWithM_ run getTouches ints
        where run touch i = do
                let ix = fromIntegral i
                when (ix ==? current) $ do
                    handle touch


average :: IFloat -> IFloat -> IFloat
average a b = do
    let alpha = 0.1
    a * (1 - alpha) + b * alpha


forceSyncDInputs :: KnownNat n => Touches n -> Ivory eff ()
forceSyncDInputs Touches{..} = do
    arrayMap $ \ix -> store (( DT.dtouches getDTouches ! ix) ~> DT.synced) false



-- manageDInputs :: KnownNat n => Touches n -> Ivory eff ()
-- manageDInputs Touches{..} = zipWithM_ zip getTouches ints
--     where
--         zip :: I.Touch i => i -> Int -> Ivory eff ()
--         zip touch i = do
--             let ix = fromIntegral i
--             let dt = DT.dtouches getDTouches ! ix
--             manageDInput dt touch



-- manageDInput :: I.Touch i
--              => Record DT.DTouchStruct
--              -> i
--              -> Ivory eff ()
-- manageDInput di input = do
--     state0 <- deref $ di ~> DT.state
--     state1 <- (/=? zero) <$> get input
--     when (state1 /=? state0) $ do
--         store (di ~> DT.state    ) state1
--         store (di ~> DT.synced   ) false



-- syncDInputs :: KnownNat n => DInputs n -> Ivory (ProcEffects s ()) ()
-- syncDInputs dis@DInputs{..} = do
--     i <- deref currentTouch
--     syncDInput dis i
--     store currentTouch $ i + 1



-- syncDInput :: KnownNat n => DInputs n -> Uint8 -> Ivory (ProcEffects s ()) ()
-- syncDInput DInputs{..} i = do
--     let n = fromIntegral $ length getInputs
--     let di = DI.dinputs getDInputs ! toIx i
--     synced <- deref $ di ~> DI.synced
--     when (iNot synced) $ do
--         msg <- DI.message getDInputs (i .% n)
--         transmit msg
--         store (di ~> DI.synced) true
