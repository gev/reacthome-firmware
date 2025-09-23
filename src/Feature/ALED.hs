{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use for_" #-}

module Feature.ALED where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import Core.Domain as D
import Core.Handler
import Core.Task
import Core.Transport (LazyTransport (lazyTransmit))
import Data.Buffer
import Data.Record
import Data.Value
import qualified Endpoint.ALED as E
import qualified Endpoint.ALED.Animation as E
import qualified Endpoint.ALED.Animation.Data as E
import GHC.TypeNats
import Interface.Display (Display, Render (Render))
import Interface.Flash as F
import Interface.MCU
import Ivory.Language
import Ivory.Language.Proxy
import Ivory.Stdlib
import Util.CRC16
import Util.Random

dt = 1 / safeCast E.fps :: IFloat

data ALED ng ns np
    = forall d f t.
      (Display d, Flash f, LazyTransport t) =>
    ALED
    { display :: d
    , getALED :: E.ALED ng ns np
    , etc :: f
    , transport :: t
    , shouldSaveConfig :: Value IBool
    , shouldSyncGroups :: Value IBool
    , shouldInit :: Value IBool
    , groupIndex :: Value Uint8
    , stateIndex :: Value (Ix ng)
    , segmentIndex :: Value (Ix ns)
    }

aled ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , Display d
    , Handler (Render np) d
    , KnownNat ng
    , KnownNat ns
    , KnownNat np
    , LazyTransport t
    , Flash f
    ) =>
    (p -> m d) ->
    (p -> f) ->
    t ->
    m (ALED ng ns np)
aled mkDisplay etc transport = do
    mcu <- asks D.mcu
    display <- mkDisplay $ peripherals mcu
    getALED <- E.mkALED
    shouldInit <- asks D.shouldInit
    shouldSaveConfig <- value "aled_should_save_config" false
    shouldSyncGroups <- value "aled_should_sync_groups" false
    groupIndex <- value "aled_group_index" 0
    stateIndex <- value "aled_state_index" 0
    segmentIndex <- value "aled_segment_index" 0

    let aled =
            ALED
                { display
                , getALED
                , etc = etc (peripherals mcu)
                , transport
                , shouldSaveConfig
                , shouldSyncGroups
                , groupIndex
                , stateIndex
                , segmentIndex
                , shouldInit
                }

    random <- mkRandom "aled" 1

    addInit "aled_load_config" $ loadConfig aled

    addHandler
        $ Render
            display
            E.fps
            (E.subPixels getALED)
        $ update aled random

    addTask $ delay 100 "save_config" $ saveConfig aled
    addTask $ delayPhase 40 35 "sync_state" $ syncState aled
    addTask $ delayPhase 40 36 "sync_groups" $ syncGroups aled

    pure aled

update ::
    forall s ng ns np.
    (KnownNat ng, KnownNat ns, KnownNat np) =>
    ALED ng ns np ->
    Random Uint8 ->
    Ivory (ProcEffects s ()) IBool
update ALED{..} random = do
    let np' = fromIntegral $ fromTypeNat (aNat :: NatType np)
    sx <- local (ival 0)
    px <- local (ival 0)
    shouldUpdate <- local $ ival false
    arrayMap $ \gx -> do
        tx <- local (ival 0)

        let colorAnimation = E.colorAnimations getALED ! gx
        let maskAnimation = E.maskAnimations getALED ! gx
        let group = E.groups getALED ! gx
        let clip = E.clips getALED ! gx

        pixelSize' <- deref $ group ~> E.pixelSize
        groupSize' <- deref $ group ~> E.groupSize
        segmentNumber' <- deref $ group ~> E.segmentNumber
        brightness' <- deref $ group ~> E.brightness
        state' <- deref $ group ~> E.groupState

        start' <- deref $ clip ~> E.start
        end' <- deref $ clip ~> E.end
        inverse' <- deref $ clip ~> E.inverse

        colorAnimationSplit' <- deref $ colorAnimation ~> E.split
        maskAnimationSplit' <- deref $ maskAnimation ~> E.split

        state <- local $ ival false

        for (toIx segmentNumber' :: Ix ns) $ \segmentX -> do
            sx' <- deref sx
            let segment = E.segments getALED ! sx'
            segmentSize' <- deref $ segment ~> E.segmentSize
            direction' <- deref $ segment ~> E.direction
            tx' <- deref tx
            store tx $ tx' + safeCast segmentSize'
            let start'' = start' * safeCast segmentSize'
            let end'' = end' * safeCast segmentSize'
            for (toIx segmentSize' :: Ix np) $ \pixelX -> do
                let pixelX' = fromIx pixelX
                x <- local $ ival pixelX'
                when direction' $ store x (safeCast segmentSize' - pixelX' - 1)
                x' <- deref x
                ifte_
                    (brightness' >? 0 .&& (safeCast x' >=? start'' .&& safeCast x' <? end'') /=? inverse')
                    ( do
                        m' <-
                            ifte
                                maskAnimationSplit'
                                ( E.renderMask
                                    random
                                    maskAnimation
                                    (fromIx segmentX)
                                    (safeCast segmentSize')
                                    x'
                                )
                                ( E.renderMask
                                    random
                                    maskAnimation
                                    0
                                    groupSize'
                                    (x' + tx')
                                )
                        for (toIx pixelSize' :: Ix np) $ \subpixelX -> do
                            px' <- deref px
                            let p = E.subPixels getALED ! px'
                            p' <- safeCast <$> deref p
                            c' <-
                                ifte
                                    colorAnimationSplit'
                                    ( E.renderColor
                                        brightness'
                                        random
                                        colorAnimation
                                        (fromIx segmentX)
                                        (safeCast segmentSize')
                                        x'
                                        pixelSize'
                                        (fromIx subpixelX)
                                        p'
                                    )
                                    ( E.renderColor
                                        brightness'
                                        random
                                        colorAnimation
                                        0
                                        groupSize'
                                        (x' + tx')
                                        pixelSize'
                                        (fromIx subpixelX)
                                        p'
                                    )
                            let v' = c' * m'
                            p' <- deref p
                            cond_
                                [ v' >? 255 ==> store p 255 >> store state true
                                , v' >? 0 ==> store p (castDefault v') >> store state true
                                , true ==> store p 0
                                ]
                            p'' <- deref p
                            when (p'' /=? p') $ store shouldUpdate true
                            store px $ px' + 1
                    )
                    ( for (toIx pixelSize' :: Ix np) . const $ do
                        px' <- deref px
                        let p = E.subPixels getALED ! px'
                        p' <- deref p
                        when (p' /=? 0) $ store shouldUpdate true
                        store p 0
                        store px $ px' + 1
                    )
            store sx $ sx' + 1
        state' <- deref $ group ~> E.groupState
        state'' <- deref state
        when (state' /=? state'') $ do
            store (group ~> E.stateChanged) true
            store (group ~> E.groupState) state''
        incrementTime colorAnimation
        incrementTime maskAnimation
    px' <- deref px
    upTo px' (np' - 1) $ \ix ->
        store (E.subPixels getALED ! ix) 0

    deref shouldUpdate

incrementTime :: Record E.AnimationStruct -> Ivory eff ()
incrementTime animation = do
    animationState' <- deref $ animation ~> E.animationState
    when animationState' $ do
        loop' <- deref $ animation ~> E.animationLoop
        time' <- deref $ animation ~> E.time
        timeEnd' <- deref $ animation ~> E.timeEnd
        dt' <- deref $ animation ~> E.dt
        let time'' = time' + dt'
        when (time'' >=? 0 .&& loop') $ store (animation ~> E.inLoop) true
        ifte_
            (time'' >=? timeEnd')
            ( ifte_
                loop'
                ( do
                    store (animation ~> E.time) 0
                    store (animation ~> E.timeEnd) 1
                    store (animation ~> E.inLoop) true
                    store (animation ~> E.startLoop) true
                )
                (store (animation ~> E.animationState) false)
            )
            ( do
                store (animation ~> E.time) time''
                store (animation ~> E.startLoop) false
            )

onALedOn ::
    forall n ng ns np s t.
    (KnownNat n, KnownNat ng) =>
    ALED ng ns np ->
    Buffer n Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onALedOn = testAnimation 0x13 false

onALedOff ::
    forall n ng ns np s t.
    (KnownNat n, KnownNat ng) =>
    ALED ng ns np ->
    Buffer n Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onALedOff = testAnimation 0x03 true

testAnimation ::
    forall n ng ns np s t.
    (KnownNat n, KnownNat ng) =>
    Uint8 ->
    IBool ->
    ALED ng ns np ->
    Buffer n Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
testAnimation animation inverseDirection ALED{..} buff size = do
    let ng' = fromIntegral $ fromTypeNat (aNat :: NatType ng)
    when (size ==? 2) $ do
        i <- deref $ buff ! 1
        when (i >=? 1 .&& i <=? ng') $ do
            let ix = toIx $ i - 1

            let clip = E.clips getALED ! ix
            store (clip ~> E.start) 0
            store (clip ~> E.end) 1
            store (clip ~> E.inverse) false

            let colorAnimation = E.colorAnimations getALED ! ix
            store (colorAnimation ~> E.kind) 0x20
            store (colorAnimation ~> E.phase) 0
            store (colorAnimation ~> E.time) 0
            store (colorAnimation ~> E.timeEnd) 1
            store (colorAnimation ~> E.dt) dt
            store (colorAnimation ~> E.split) true
            store (colorAnimation ~> E.inLoop) false
            store (colorAnimation ~> E.animationLoop) true
            store (colorAnimation ~> E.animationState) true
            store (colorAnimation ~> E.inverseDirection) false
            store (colorAnimation ~> E.params ! 0) 0
            store (colorAnimation ~> E.params ! 1) 255
            store (colorAnimation ~> E.params ! 2) 0
            store (colorAnimation ~> E.params ! 3) 255
            store (colorAnimation ~> E.params ! 4) 0
            store (colorAnimation ~> E.params ! 5) 255
            store (colorAnimation ~> E.params ! 6) 0
            store (colorAnimation ~> E.params ! 7) 255

            let maskAnimation = E.maskAnimations getALED ! ix
            store (maskAnimation ~> E.kind) animation
            store (maskAnimation ~> E.phase) 0
            store (maskAnimation ~> E.time) 0
            store (maskAnimation ~> E.timeEnd) 1
            store (maskAnimation ~> E.dt) dt
            store (maskAnimation ~> E.split) true
            store (maskAnimation ~> E.inLoop) false
            store (maskAnimation ~> E.animationLoop) false
            store (maskAnimation ~> E.animationState) true
            store (maskAnimation ~> E.inverseDirection) inverseDirection

onALedColorAnimationPlay ::
    forall n ng ns np s t.
    (KnownNat n, KnownNat ng) =>
    ALED ng ns np ->
    Buffer n Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onALedColorAnimationPlay ALED{..} =
    onALedAnimationPlay (E.colorAnimations getALED) (E.groups getALED) transport

onALedColorAnimationStop ::
    forall n ng ns np s t.
    (KnownNat n, KnownNat ng) =>
    ALED ng ns np ->
    Buffer n Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onALedColorAnimationStop ALED{..} =
    onALedAnimationStop (E.colorAnimations getALED) transport

onALedMaskAnimationPlay ::
    forall n ng ns np s t.
    (KnownNat n, KnownNat ng) =>
    ALED ng ns np ->
    Buffer n Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onALedMaskAnimationPlay ALED{..} =
    onALedAnimationPlay (E.maskAnimations getALED) (E.groups getALED) transport

onALedMaskAnimationStop ::
    forall n ng ns np s t.
    (KnownNat n, KnownNat ng) =>
    ALED ng ns np ->
    Buffer n Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onALedMaskAnimationStop ALED{..} =
    onALedAnimationStop (E.maskAnimations getALED) transport

onALedAnimationPlay ::
    forall n ng s r t.
    (KnownNat n, KnownNat ng, LazyTransport t) =>
    Records ng E.AnimationStruct ->
    Records ng E.GroupStruct ->
    t ->
    Buffer n Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s r) ()
onALedAnimationPlay animations groups transport buff size = do
    let ng' = fromIntegral $ fromTypeNat (aNat :: NatType ng)
    when (size >=? 8 .&& size <=? 20) $ do
        i <- deref $ buff ! 1
        when (i >=? 1 .&& i <=? ng') $ do
            let ix = toIx $ i - 1

            let animation = animations ! ix

            kind <- deref $ buff ! 2
            duration <- deref $ buff ! 3
            phase <- deref $ buff ! 4
            split <- deref $ buff ! 5
            loop <- deref $ buff ! 6
            inverseDirection <- deref $ buff ! 7

            let n = size - 8
            let params = animation ~> E.params

            arrayMap $ \ix -> store (params ! ix) 0

            arrayMap $ \ix ->
                ifte_
                    (fromIx ix <? safeCast n)
                    (store (params ! ix) =<< deref (buff ! toIx (fromIx ix + 8)))
                    (store (params ! ix) 0)

            kind' <- deref $ animation ~> E.kind
            store (animation ~> E.kind) kind

            let dt' = 4 * dt / (safeCast duration + 1)
            let phase' = dt' * (safeCast phase - 128)

            store (animation ~> E.dt) dt'
            store (animation ~> E.phase) phase'

            store (animation ~> E.animationState) true

            ifte_
                (split ==? 0)
                (store (animation ~> E.split) false)
                (store (animation ~> E.split) true)
            ifte_
                (loop ==? 0)
                (store (animation ~> E.animationLoop) false)
                (store (animation ~> E.animationLoop) true)
            ifte_
                (inverseDirection ==? 0)
                (store (animation ~> E.inverseDirection) false)
                (store (animation ~> E.inverseDirection) true)

            store (animation ~> E.inLoop) false
            store (animation ~> E.startLoop) true

            when (loop ==? 0 .|| kind' /=? kind) $ do
                segmentNumber' <- deref $ (groups ! ix) ~> E.segmentNumber
                let additionalTime = phase' * (safeCast segmentNumber' - 1)
                cond_
                    [ additionalTime >? 0 ==> do
                        store (animation ~> E.time) 0
                        store (animation ~> E.timeEnd) $ 1 + additionalTime
                    , additionalTime <? 0 ==> do
                        store (animation ~> E.time) additionalTime
                        store (animation ~> E.timeEnd) 1
                    , true ==> do
                        store (animation ~> E.time) 0
                        store (animation ~> E.timeEnd) 1
                    ]

            lazyTransmit transport 20 $ \transmit -> do
                transmit =<< deref (buff ! 0)
                transmit i
                transmit kind
                transmit duration
                transmit phase
                transmit split
                transmit loop
                transmit inverseDirection
                arrayMap $ \ix -> transmit =<< deref (params ! ix)

onALedAnimationStop ::
    forall n ng s r t.
    (KnownNat n, KnownNat ng, LazyTransport t) =>
    Records ng E.AnimationStruct ->
    t ->
    Buffer n Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s r) ()
onALedAnimationStop animations transport buff size = do
    let ng' = fromIntegral $ fromTypeNat (aNat :: NatType ng)
    when (size ==? 2) $ do
        i <- deref $ buff ! 1
        when (i >=? 1 .&& i <=? ng') $ do
            let ix = toIx $ i - 1
            store (animations ! ix ~> E.animationState) false
            lazyTransmit transport size $ \transmit -> do
                for (toIx size) $ \ix -> transmit =<< deref (buff ! ix)

onALedBrightness ::
    forall n ng ns np s t.
    (KnownNat n, KnownNat ng) =>
    ALED ng ns np ->
    Buffer n Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onALedBrightness ALED{..} buff size = do
    let ng' = fromIntegral $ fromTypeNat (aNat :: NatType ng)
    when (size ==? 3) $ do
        i <- deref $ buff ! 1
        when (i >=? 1 .&& i <=? ng') $ do
            let ix = toIx $ i - 1
            brightness <- deref $ buff ! 2
            store (E.groups getALED ! ix ~> E.brightness) $ safeCast brightness / 255
            lazyTransmit transport size $ \transmit -> do
                transmit actionALedBrightness
                transmit i
                transmit brightness

onALedClip ::
    forall n ng ns np s t.
    (KnownNat n, KnownNat ng) =>
    ALED ng ns np ->
    Buffer n Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onALedClip ALED{..} buff size = do
    let ng' = fromIntegral $ fromTypeNat (aNat :: NatType ng)
    when (size ==? 5) $ do
        i <- deref $ buff ! 1
        when (i >=? 1 .&& i <=? ng') $ do
            let ix = toIx $ i - 1
            let clip = E.clips getALED ! ix
            start <- deref $ buff ! 2
            end <- deref $ buff ! 3
            inverse <- deref $ buff ! 4
            store (clip ~> E.start) $ safeCast start / 255
            store (clip ~> E.end) $ safeCast end / 255
            ifte_
                (inverse ==? 0)
                (store (clip ~> E.inverse) false)
                (store (clip ~> E.inverse) true)
            lazyTransmit transport 5 $ \transmit -> do
                transmit actionALedClip
                transmit i
                transmit start
                transmit end
                transmit inverse

onALedConfigGroup ::
    forall n ng ns np s t.
    (KnownNat n, KnownNat ng, KnownNat ns) =>
    ALED ng ns np ->
    Buffer n Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onALedConfigGroup a@ALED{..} buff size = do
    let ng' = fromIntegral $ fromTypeNat (aNat :: NatType ng)
    let ns' = fromIntegral $ fromTypeNat (aNat :: NatType ns)

    when (size >=? 4) $ do
        i <- deref $ buff ! 1

        when (i >=? 1 .&& i <=? ng') $ do
            let index = i - 1

            n <- local $ ival 0
            colors <- deref $ buff ! 2
            store n =<< deref (buff ! 3)

            let ix' = toIx index
            let group = E.groups getALED ! ix'

            store (group ~> E.colors) colors
            cond_
                [ colors ==? 2 ==> store (group ~> E.pixelSize) 3
                , colors ==? 3 ==> store (group ~> E.pixelSize) 3
                , colors ==? 4 ==> store (group ~> E.pixelSize) 4
                , true ==> store (group ~> E.pixelSize) 0
                ]

            endSegment <- local $ ival 0
            startSegment <- local $ ival 0
            arrayMap $ \ix -> do
                endSegment' <- deref endSegment
                when (ix <=? ix') $ store startSegment endSegment'
                segmentNumber' <- deref $ E.groups getALED ! ix ~> E.segmentNumber
                store endSegment $ endSegment' + segmentNumber'
            startSegment' <- deref startSegment
            endSegment' <- deref endSegment

            segmentNumber <- deref n
            segmentNumber' <- deref $ group ~> E.segmentNumber

            when (segmentNumber <? segmentNumber') $ do
                let shift = segmentNumber' - segmentNumber
                upTo (toIx $ startSegment' + segmentNumber) (toIx $ ns' - shift - 1) $ \ix -> do
                    let to = E.segments getALED ! ix
                    let from = E.segments getALED ! (ix + toIx shift)
                    store (to ~> E.direction) =<< deref (from ~> E.direction)
                    store (to ~> E.segmentSize) =<< deref (from ~> E.segmentSize)

            when (segmentNumber >? segmentNumber') $ do
                shift <- local . ival $ segmentNumber - segmentNumber'
                shift' <- deref shift
                let free = ns' - endSegment'
                when (free <? shift') $ store n (segmentNumber' + free)
                segmentNumber <- deref n
                when (segmentNumber >? segmentNumber') $ do
                    let shift = segmentNumber - segmentNumber'
                    downTo (toIx $ ns' - shift - 1) (toIx $ startSegment' + segmentNumber') $ \ix -> do
                        let from = E.segments getALED ! ix
                        let to = E.segments getALED ! (ix + toIx shift)
                        store (to ~> E.direction) =<< deref (from ~> E.direction)
                        store (to ~> E.segmentSize) =<< deref (from ~> E.segmentSize)

            segmentNumber <- deref n
            store (group ~> E.segmentNumber) segmentNumber

            jx <- local $ ival 4
            store (group ~> E.groupSize) 0
            upTo (toIx startSegment') (toIx (startSegment' + segmentNumber) - 1) $ \ix -> do
                let segment = E.segments getALED ! ix
                jx' <- deref jx
                direction <- deref $ buff ! jx'
                ifte_
                    (direction ==? 0)
                    (store (segment ~> E.direction) false)
                    (store (segment ~> E.direction) true)
                segmentSize' <- deref $ buff ! (jx' + 1)
                store (segment ~> E.segmentSize) segmentSize'
                groupSize' <- deref $ group ~> E.groupSize
                store (group ~> E.groupSize) $ groupSize' + safeCast segmentSize'
                store jx $ jx' + 2

            store shouldSaveConfig true

            lazyTransmit transport size $ \transmit ->
                for (toIx size) $ \ix ->
                    ifte_
                        (ix ==? 3)
                        (transmit segmentNumber)
                        (transmit =<< deref (buff ! ix))

transmitNextGroup ::
    forall n ng ns np s t.
    (KnownNat ng, KnownNat ns) =>
    ALED ng ns np ->
    Ivory (ProcEffects s t) ()
transmitNextGroup ALED{..} = do
    groupIndex' <- deref groupIndex
    let gx = toIx groupIndex'
    let group = E.groups getALED ! gx
    segmentNumber' <- deref $ group ~> E.segmentNumber
    let size = 4 + 2 * segmentNumber'
    lazyTransmit transport size $ \transmit -> do
        transmit actionALedConfigGroup
        transmit $ 1 + groupIndex'
        transmit =<< deref (group ~> E.colors)
        transmit segmentNumber'
        for (toIx segmentNumber' :: Ix ns) $ \ix -> do
            sx <- deref segmentIndex
            let segment = E.segments getALED ! sx
            direction' <- deref $ segment ~> E.direction
            segmentSize' <- deref $ segment ~> E.segmentSize
            ifte_
                direction'
                (transmit 1)
                (transmit 0)
            transmit segmentSize'
            store segmentIndex $ sx + 1
    store groupIndex $ groupIndex' + 1

syncState ::
    forall n ng ns np s t.
    (KnownNat ng) =>
    ALED ng ns np ->
    Ivory (ProcEffects s t) ()
syncState a@ALED{..} = do
    ix <- deref stateIndex
    let group = E.groups getALED ! ix
    stateChanged' <- deref $ group ~> E.stateChanged
    when stateChanged' $ do
        lazyTransmit transport 2 $ \transmit -> do
            state' <- deref $ group ~> E.groupState
            ifte_
                state'
                (transmit actionALedOn)
                (transmit actionALedOff)
            transmit . castDefault $ 1 + fromIx ix
        store (group ~> E.stateChanged) false
    store stateIndex $ ix + 1

syncGroups ::
    forall n ng ns np s t.
    (KnownNat ng, KnownNat ns) =>
    ALED ng ns np ->
    Ivory (ProcEffects s t) ()
syncGroups a@ALED{..} = do
    shouldSyncGroups' <- deref shouldSyncGroups
    when shouldSyncGroups' $ do
        let ng' = fromIntegral $ fromTypeNat (aNat :: NatType ng)
        groupIndex' <- deref groupIndex
        ifte_
            (groupIndex' <? ng')
            (transmitNextGroup a)
            (store shouldSyncGroups false)

forceSyncAled ::
    (KnownNat ng, KnownNat ns) =>
    ALED ng ns np ->
    Ivory (ProcEffects s t) ()
forceSyncAled ALED{..} = do
    store groupIndex 0
    store segmentIndex 0
    store shouldSyncGroups true
    arrayMap $ \ix ->
        store (E.groups getALED ! ix ~> E.stateChanged) true
    store stateIndex 0

onInitialize ::
    forall n ng ns np s t.
    (KnownNat n, KnownNat ng) =>
    ALED ng ns np ->
    Buffer n Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onInitialize ALED{..} buff size = do
    let ng' = fromIntegral $ fromTypeNat (aNat :: NatType ng)
    when (size ==? 1 + ng') $ do
        arrayMap $ \ix -> do
            let group = E.groups getALED ! ix
            brightness <- deref $ buff ! toIx (fromIx ix + 1)
            store (group ~> E.brightness) $ safeCast brightness / 255
        store shouldInit false

saveConfig ::
    (KnownNat ng, KnownNat ns) =>
    ALED ng ns np ->
    Ivory (ProcEffects s t) ()
saveConfig ALED{..} = do
    shouldSaveConfig' <- deref shouldSaveConfig

    when shouldSaveConfig' $ do
        erasePage etc 0
        offset <- local $ ival 8
        crc <- local $ istruct initCRC16

        let save v = do
                offset' <- deref offset
                updateCRC16 crc v
                write etc offset' $ safeCast v
                store offset $ offset' + 4

        arrayMap $ \ix -> do
            let segment = E.segments getALED ! ix
            save =<< deref (segment ~> E.segmentSize)
            direction <- deref $ segment ~> E.direction
            ifte_ direction (save 1) (save 0)

        arrayMap $ \ix -> do
            let group = E.groups getALED ! ix
            save =<< deref (group ~> E.colors)
            save =<< deref (group ~> E.pixelSize)
            save =<< deref (group ~> E.segmentNumber)

        write etc 0 . safeCast =<< deref (crc ~> msb)
        write etc 4 . safeCast =<< deref (crc ~> lsb)

        store shouldSaveConfig false

loadConfig ::
    forall s t ng ns np.
    (KnownNat ng, KnownNat ns) =>
    ALED ng ns np ->
    Ivory (ProcEffects s t) ()
loadConfig ALED{..} = do
    offset <- local $ ival 8
    crc <- local $ istruct initCRC16
    msb'' <- F.read etc 0
    lsb'' <- F.read etc 4

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
            load = do
                offset' <- deref offset
                store offset $ offset' + 4
                castDefault <$> F.read etc offset'

        arrayMap $ \ix -> do
            let segment = E.segments getALED ! ix
            store (segment ~> E.segmentSize) =<< load
            direction :: Uint8 <- load
            ifte_
                (direction ==? 0)
                (store (segment ~> E.direction) false)
                (store (segment ~> E.direction) true)

        jx <- local $ ival 0
        arrayMap $ \ix -> do
            let group = E.groups getALED ! ix
            store (group ~> E.colors) =<< load
            store (group ~> E.pixelSize) =<< load
            segmentNumber' <- load
            store (group ~> E.segmentNumber) segmentNumber'
            store (group ~> E.groupSize) 0
            for (toIx segmentNumber' :: Ix ns) $ \_ -> do
                jx' <- deref jx
                segmentSize' <- deref $ E.segments getALED ! jx' ~> E.segmentSize
                groupSize' <- deref $ group ~> E.groupSize
                store (group ~> E.groupSize) $ groupSize' + safeCast segmentSize'
                store jx $ jx' + 1
