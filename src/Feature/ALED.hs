{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.ALED where

import           Control.Monad.Reader         (MonadReader, asks)
import           Control.Monad.State          (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Domain                  as D
import           Core.FSM                     (transit)
import           Core.Handler
import           Core.Task
import           Core.Transport               (LazyTransport (lazyTransmit))
import qualified Core.Transport               as T
import           Data.Buffer
import           Data.Display.Canvas1D
import           Data.Record
import           Data.Serialize
import           Data.Value
import           Endpoint.ALED                (brightness)
import qualified Endpoint.ALED                as E
import qualified Endpoint.ALED.Animation      as E
import           Endpoint.ALED.Animation.Data (animationState)
import qualified Endpoint.ALED.Animation.Data as E
import           GHC.TypeNats
import           Interface.Display            (Display, Render (Render))
import           Interface.Flash              as F
import           Interface.Mac
import           Interface.MCU
import           Ivory.Language
import           Ivory.Language.Proxy
import           Ivory.Stdlib
import           Util.CRC16
import           Util.Random



fps = 24 :: Uint32

dt = 1 / safeCast fps :: IFloat


data ALED ng ns np = forall d f t. (Display d, Flash f, T.LazyTransport t) => ALED
    { display          :: d
    , getALED          :: E.ALED ng ns np
    , etc              :: f
    , transport        :: t
    , shouldSaveConfig :: Value IBool
    , shouldSyncGroups :: Value IBool
    , groupIndex       :: Value Uint8
    , segmentIndex     :: Value (Ix ns)
    }



maxValue = 0.3 :: IFloat

aled :: ( MonadState Context m
        , MonadReader (D.Domain p c) m
        , Display d, Handler (Render np) d
        , KnownNat ng, KnownNat ns, KnownNat np
        , T.LazyTransport t
        , Flash f
        ) => (p -> m d) -> ( p-> f) -> t -> m (ALED ng ns np)
aled mkDisplay etc transport = do
    mcu              <- asks D.mcu
    display          <- mkDisplay $ peripherals mcu
    getALED          <- E.mkALED
    shouldSaveConfig <- value "should_save_config" false
    shouldSyncGroups <- value "should_sync_groups" false
    groupIndex       <- value "group_index" 0
    segmentIndex     <- value "segment_index" 0

    let aled = ALED { display
                    , getALED
                    , etc = etc (peripherals mcu)
                    , transport
                    , shouldSaveConfig
                    , shouldSyncGroups
                    , groupIndex
                    , segmentIndex
                    }

    random <- mkRandom "aled" 1

    addInit "aed_load_config" $ do
        store (E.animations getALED ! 0 ~> E.kind) 2
        store (E.animations getALED ! 0 ~> E.dt) $ dt / 5
        store (E.animations getALED ! 0 ~> E.animationState) true
        store (E.animations getALED ! 0 ~> E.animationLoop) true
        store (E.animations getALED ! 0 ~> E.params ! 0) 64
        -- store (E.animations getALED ! 0 ~> E.params ! 0) 0
        -- store (E.animations getALED ! 0 ~> E.params ! 1) 64
        -- store (E.animations getALED ! 0 ~> E.params ! 2) 128
        -- store (E.animations getALED ! 0 ~> E.params ! 3) 192

        store (E.animations getALED ! 1 ~> E.kind) 2
        store (E.animations getALED ! 1 ~> E.dt) $ dt / 5
        store (E.animations getALED ! 1 ~> E.animationState) true
        store (E.animations getALED ! 1 ~> E.animationLoop) true
        store (E.animations getALED ! 1 ~> E.params ! 0) 128
        -- store (E.animations getALED ! 1 ~> E.params ! 0) 0
        -- store (E.animations getALED ! 1 ~> E.params ! 1) 85
        -- store (E.animations getALED ! 1 ~> E.params ! 2) 170

        loadConfig aled

    addHandler $ Render display fps (E.subPixels getALED) $ do
        update aled random

    addTask $ delay 100 "save_config" $ saveConfig aled
    addTask $ delay  20 "sync_groups" $ syncGroups aled

    -- addTask $ delay 4000 "player" $ do
    --     store (E.animations getALED ! 0 ~> E.time) 0
    --     store (E.animations getALED ! 0 ~> E.dt) $ dt / 2
    --     store (E.animations getALED ! 0 ~> E.animationState) true
    --     store (E.animations getALED ! 0 ~> E.animationLoop) false
    --     store (E.animations getALED ! 0 ~> E.params ! 0) =<< next random
    --     store (E.animations getALED ! 0 ~> E.params ! 1) =<< next random
    --     store (E.animations getALED ! 0 ~> E.params ! 2) =<< next random
    --     store (E.animations getALED ! 0 ~> E.params ! 3) =<< next random


    pure aled



update :: forall s ng ns np. (KnownNat ng, KnownNat ns, KnownNat np)
       => ALED ng ns np -> Random Uint8 -> Ivory (ProcEffects s ()) ()
update ALED{..} random = do
    let np' = fromIntegral $ fromTypeNat (aNat :: NatType np)
    sx <- local (ival 0)
    px <- local (ival 0)
    arrayMap $ \gx -> do
        let animation   = E.animations getALED ! gx
        let group       = E.groups getALED ! gx

        pixelSize'     <- deref $ group ~> E.pixelSize
        segmentNumber' <- deref $ group ~> E.segmentNumber
        brightness'    <- deref $ group ~> E.brightness
        state'         <- deref $ group ~> E.groupState

        for (toIx segmentNumber' :: Ix ns) $ \segmentX -> do
            sx' <- deref sx
            let segment   = E.segments getALED ! sx'
            segmentSize' <- deref $ segment ~> E.segmentSize
            direction'   <- deref $ segment ~> E.direction

            for (toIx segmentSize' :: Ix np) $ \pixelX -> do
                for (toIx pixelSize' :: Ix np) $ \subpixelX -> do
                    px' <- deref px
                    let p = E.subPixels getALED ! px'
                    ifte_ (state' .&& brightness' >? 0)
                        (do
                            let pixelX' = fromIx pixelX
                            x <- local $ ival pixelX'
                            when direction' $ store x (safeCast segmentSize' - pixelX')
                            x' <- deref x
                            p' <- deref p
                            b  <- (* brightness') <$>
                                  E.renderAnimation random
                                                    animation
                                                    (fromIx segmentX)
                                                    segmentSize'
                                                    (fromIx pixelX)
                                                    pixelSize'
                                                    (fromIx subpixelX)
                                                    (safeCast p' / brightness')
                            cond_ [ b <? 0   ==> store p 0
                                  , b >? 255 ==> store p 255
                                  , true     ==> store p (castDefault b)
                                  ]
                        )
                        (store p 0)
                    store px $ px' + 1

            store sx $ sx' + 1

        animationState' <- deref $ animation ~> E.animationState
        when animationState' $ do
                time' <- deref $ animation ~> E.time
                dt'   <- deref $ animation ~> E.dt
                let time'' = time' + dt'
                ifte_ (time'' >=? 1)
                    (do
                        loop' <- deref $ animation ~> E.animationLoop
                        ifte_ loop'
                            (store (animation ~> E.time) 0)
                            (store (animation ~> E.time) 1)
                    )
                    (store (animation ~> E.time) time'')

    px' <- deref px
    upTo px' (np' - 1) $ \ix ->
        store (E.subPixels getALED ! ix) 0



onALedOn :: forall n ng ns np s t. (KnownNat n, KnownNat ng)
         => ALED ng ns np -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onALedOn ALED{..} buff size = do
    let ng' = fromIntegral $ fromTypeNat (aNat :: NatType ng)
    when (size ==? 2) $ do
        i <- deref $ buff ! 1
        when (i >=? 1 .&& i <=? ng') $ do
            let ix = toIx $ i - 1
            store (E.groups getALED ! ix ~> E.groupState) true
            T.lazyTransmit transport size $ \transmit -> do
                transmit actionALedOn
                transmit i



onALedOff :: forall n ng ns np s t. (KnownNat n, KnownNat ng)
          => ALED ng ns np -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onALedOff ALED{..} buff size = do
    let ng' = fromIntegral $ fromTypeNat (aNat :: NatType ng)
    when (size ==? 2) $ do
        i <- deref $ buff ! 1
        when (i >=? 1 .&& i <=? ng') $ do
            let ix = toIx $ i - 1
            store (E.groups getALED ! ix ~> E.groupState) false
            T.lazyTransmit transport size $ \transmit -> do
                transmit actionALedOff
                transmit i



onALedPlay :: forall n ng ns np s t. (KnownNat n, KnownNat ng)
           => ALED ng ns np -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onALedPlay ALED{..} buff size = do
    let ng' = fromIntegral $ fromTypeNat (aNat :: NatType ng)
    when (size ==? 3) $ do
        i <- deref $ buff ! 1
        when (i >=? 1 .&& i <=? ng') $ do
            let ix = toIx $ i - 1
            loop  <- deref $ buff ! 2
            let animation = E.animations getALED ! ix
            store (animation ~> E.animationState) true
            ifte_ (loop ==? 0)
                (store (animation ~> E.animationLoop) false)
                (store (animation ~> E.animationLoop) true)
            T.lazyTransmit transport size $ \transmit -> do
                transmit actionALedPlay
                transmit i
                transmit loop



onALedStop :: forall n ng ns np s t. (KnownNat n, KnownNat ng)
           => ALED ng ns np -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onALedStop ALED{..} buff size = do
    let ng' = fromIntegral $ fromTypeNat (aNat :: NatType ng)
    when (size ==? 2) $ do
        i <- deref $ buff ! 1
        when (i >=? 1 .&& i <=? ng') $ do
            let ix = toIx $ i - 1
            store (E.animations getALED ! ix ~> E.animationState) false
            T.lazyTransmit transport 2 $ \transmit -> do
                transmit actionALedStop
                transmit i



onALedBrightness :: forall n ng ns np s t. (KnownNat n, KnownNat ng)
                 => ALED ng ns np -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onALedBrightness ALED{..} buff size = do
    let ng' = fromIntegral $ fromTypeNat (aNat :: NatType ng)
    when (size ==? 3) $ do
        i <- deref $ buff ! 1
        when (i >=? 1 .&& i <=? ng') $ do
            let ix = toIx $ i - 1
            brightness <- deref $ buff ! 2
            store (E.groups getALED ! ix ~> E.brightness) $ safeCast brightness / 255
            T.lazyTransmit transport size $ \transmit -> do
                transmit actionALedBrightness
                transmit i
                transmit brightness



onALedConfigGroup :: forall n ng ns np s t. (KnownNat n, KnownNat ng, KnownNat ns)
                  => ALED ng ns np -> Buffer n Uint8 -> Uint8
                  -> Ivory (ProcEffects s t) ()
onALedConfigGroup a@ALED{..} buff size = do
    let ng' = fromIntegral $ fromTypeNat (aNat :: NatType ng)
    let ns' = fromIntegral $ fromTypeNat (aNat :: NatType ns)

    when (size >=? 4) $ do

        i <-  deref $ buff ! 1

        when (i >=? 1 .&& i <=? ng') $ do

            let index = i - 1

            n       <-  local $ ival 0
            colors  <-  deref $ buff ! 2
            store n =<< deref  (buff ! 3)

            let ix' = toIx index
            let group = E.groups getALED ! ix'

            store (group ~> E.colors) colors
            cond_ [ colors ==? 2 ==> store (group ~> E.pixelSize) 3
                  , colors ==? 3 ==> store (group ~> E.pixelSize) 3
                  , colors ==? 4 ==> store (group ~> E.pixelSize) 4
                  , true         ==> store (group ~> E.pixelSize) 0
                  ]

            endSegment   <- local $ ival 0
            startSegment <- local $ ival 0
            arrayMap $ \ix -> do
                endSegment' <- deref endSegment
                when (ix <=? ix') $ store startSegment endSegment'
                segmentNumber' <- deref $ E.groups getALED ! ix ~> E.segmentNumber
                store endSegment $ endSegment' + segmentNumber'
            startSegment' <- deref startSegment
            endSegment'   <- deref endSegment

            segmentNumber  <- deref n
            segmentNumber' <- deref $ group ~> E.segmentNumber

            when (segmentNumber <? segmentNumber') $ do
                let shift = segmentNumber' - segmentNumber
                upTo (toIx $ startSegment' + segmentNumber ) (toIx $ ns' - shift - 1) $ \ix -> do
                    let to   = E.segments getALED ! ix
                    let from = E.segments getALED ! (ix + toIx shift)
                    store (to ~> E.direction) =<< deref (from ~> E.direction)
                    store (to ~> E.segmentSize) =<< deref (from ~> E.segmentSize)

            when (segmentNumber >? segmentNumber') $ do
                shift  <- local . ival $ segmentNumber - segmentNumber'
                shift' <- deref shift
                let free = ns' - endSegment'
                when (free <? shift') $ store n (segmentNumber' + free)
                segmentNumber <- deref n
                when (segmentNumber >? segmentNumber') $ do
                    let shift = segmentNumber - segmentNumber'
                    downTo (toIx $ ns' - shift - 1) (toIx $ startSegment' + segmentNumber') $ \ix -> do
                        let from = E.segments getALED ! ix
                        let to   = E.segments getALED ! (ix + toIx shift)
                        store (to ~> E.direction) =<< deref (from ~> E.direction)
                        store (to ~> E.segmentSize) =<< deref (from ~> E.segmentSize)

            segmentNumber <- deref n
            store (group ~> E.segmentNumber) segmentNumber

            jx <- local $ ival 4
            upTo (toIx startSegment') (toIx (startSegment' + segmentNumber) - 1) $ \ix -> do
                let segment = E.segments getALED ! ix
                jx' <- deref jx
                direction <- deref $ buff ! jx'
                ifte_ (direction ==? 0)
                    (store (segment ~> E.direction) false)
                    (store (segment ~> E.direction) true)
                store (segment ~> E.segmentSize) =<< deref (buff ! (jx' + 1))
                store jx $ jx' + 2

            store shouldSaveConfig true

            lazyTransmit transport size $ \transmit ->
                for (toIx size) $ \ix ->
                    ifte_ (ix ==? 3)
                        (transmit segmentNumber)
                        (transmit =<< deref (buff ! ix))



transmitNextGroup :: forall n ng ns np s t. (KnownNat ng, KnownNat ns)
                  => ALED ng ns np
                  -> Ivory (ProcEffects s t) ()
transmitNextGroup ALED{..} = do
    groupIndex' <- deref groupIndex
    let gx = toIx groupIndex'
    let group = E.groups getALED ! gx
    segmentNumber' <- deref $ group ~> E.segmentNumber
    let size = 4 + 2 * segmentNumber'
    T.lazyTransmit transport size $ \transmit -> do
        transmit actionALedConfigGroup
        transmit $ 1 + groupIndex'
        transmit =<< deref (group ~> E.colors)
        transmit segmentNumber'
        for (toIx segmentNumber':: Ix ns) $ \ix -> do
            sx <- deref segmentIndex
            let segment = E.segments getALED ! sx
            direction' <- deref $ segment ~> E.direction
            segmentSize' <- deref $ segment ~> E.segmentSize
            ifte_ direction'
                (transmit 1)
                (transmit 0)
            transmit segmentSize'
            store segmentIndex $ sx + 1
    store groupIndex $ groupIndex' + 1



syncGroups :: forall n ng ns np s t. (KnownNat ng, KnownNat ns)
           => ALED ng ns np -> Ivory (ProcEffects s t) ()
syncGroups a@ALED{..} = do
    shouldSyncGroups' <- deref shouldSyncGroups
    when shouldSyncGroups' $ do
        let ng' = fromIntegral $ fromTypeNat (aNat :: NatType ng)
        groupIndex' <- deref groupIndex
        ifte_ (groupIndex' <? ng')
              (transmitNextGroup a)
              (store shouldSyncGroups false)



forceSyncAled :: (KnownNat ng, KnownNat ns)
              => ALED ng ns np -> Ivory (ProcEffects s t) ()
forceSyncAled ALED{..} = do
    store groupIndex 0
    store segmentIndex 0
    store shouldSyncGroups true



onInitialize :: forall n ng ns np s t. (KnownNat n, KnownNat ng)
             => ALED ng ns np -> Buffer n Uint8 -> Uint8
             -> Ivory (ProcEffects s t) ()
onInitialize ALED{..} buff size = do
    let ng' = fromIntegral $ fromTypeNat (aNat :: NatType ng)
    when (size ==? 1 + 2 * ng') $ do
        jx <- local $ ival 1
        arrayMap $ \ix -> do
            jx' <- toIx <$> deref jx
            let group = E.groups getALED ! ix
            brightness <- deref (buff ! jx')
            state <- deref $ buff ! (jx' + 1)
            store (group ~> E.brightness) $ safeCast brightness / 255
            ifte_ (state ==? 0)
                  (store (group ~> E.groupState) false)
                  (store (group ~> E.groupState) true)
            store jx $ jx' + 2



saveConfig :: (KnownNat ng, KnownNat ns)
           => ALED ng ns np -> Ivory (ProcEffects s t) ()
saveConfig ALED{..} = do
    shouldSaveConfig' <- deref shouldSaveConfig

    when shouldSaveConfig' $ do
        erasePage etc 0
        offset <- local $ ival 8
        crc    <- local $ istruct initCRC16

        let save v = do
                offset' <- deref offset
                updateCRC16 crc v
                write etc offset' $ safeCast v
                store offset $ offset' + 4

        arrayMap $ \ix -> do
            let group = E.groups getALED ! ix
            save =<< deref (group ~> E.colors)
            save =<< deref (group ~> E.pixelSize)
            save =<< deref (group ~> E.segmentNumber)

        arrayMap $ \ix -> do
            let segment = E.segments getALED ! ix
            save =<< deref (segment ~> E.segmentSize)
            direction <- deref $ segment ~> E.direction
            ifte_ direction (save 1) (save 0)

        write etc 0 . safeCast =<< deref (crc ~> msb)
        write etc 4 . safeCast =<< deref (crc ~> lsb)

        store shouldSaveConfig false



loadConfig :: forall s t ng ns np. (KnownNat ng, KnownNat ns)
           => ALED ng ns np -> Ivory (ProcEffects s t) ()
loadConfig ALED{..} = do
    offset <- local $ ival 8
    crc    <- local $ istruct initCRC16
    msb''  <- F.read etc 0
    lsb''  <- F.read etc 4

    let calc = do
            offset' <- deref offset
            updateCRC16 crc . castDefault =<< F.read etc offset'
            store offset $ offset' + 4

    arrayMap $ \(_ :: Ix ng) -> calc >> calc >> calc
    arrayMap $ \(_ :: Ix ns) -> calc >> calc

    msb' <- deref $ crc ~> msb
    lsb' <- deref $ crc ~> lsb

    when (msb'' ==? safeCast msb' .&& lsb'' ==? safeCast lsb') $ do

        store offset 8

        let load :: (SafeCast x Uint32, IvoryOrd x, Bounded x, Default x) => Ivory eff x
            load  = do
                offset' <- deref offset
                store offset $ offset' + 4
                castDefault <$> F.read etc offset'

        arrayMap $ \ix -> do
            let group = E.groups getALED ! ix
            store (group ~> E.colors) =<< load
            store (group ~> E.pixelSize) =<< load
            store (group ~> E.segmentNumber) =<< load

        arrayMap $ \ix -> do
            let segment = E.segments getALED ! ix
            store (segment ~> E.segmentSize) =<< load
            direction :: Uint8 <- load
            ifte_ (direction ==? 0)
                  (store (segment ~> E.direction) false)
                  (store (segment ~> E.direction) true)
