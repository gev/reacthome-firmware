{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.Smart.Top.LEDs where


import           Control.Monad.Reader     (MonadReader, asks)
import           Control.Monad.State      (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Domain              as D
import           Core.Handler
import qualified Core.Transport           as T
import           Data.Buffer
import           Data.Color
import           Data.Display.Canvas1D
import           Data.Display.FrameBuffer
import           Data.Record
import           Data.Serialize
import           Data.Value
import           Endpoint.DInputs         as DI
import           Feature.Scd40            (SCD40 (txBuff))
import           GHC.TypeNats
import           Interface.Display        (Display (transmitFrameBuffer))
import qualified Interface.Display        as I
import           Interface.MCU
import           Interface.SystemClock    (getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib



data LEDs = forall d f t. (I.Display d f t, FrameBuffer f t) => LEDs
    { display    :: d
    , canvas     :: Canvas1D 6 (f t)
    , order      :: Values   6 (Ix 6)
    , t          :: Value      Sint32
    , start      :: Value      IBool
    , findMe     :: Value      IBool
    , findMeMsg  :: Buffer   2 Uint8
    , shouldInit :: Value      IBool
    , pixels     :: Records  6 RGB
    , colors     :: Records  6 RGB
    , colorMsg   :: Buffer   5 Uint8
    , dinputs    :: DInputs
    , transmit   :: forall n s t. KnownNat n => Buffer n Uint8 -> Ivory (ProcEffects s t) ()
    }


maxValue = 0.3 :: IFloat

leds :: ( MonadState Context m
        , MonadReader (D.Domain p t c) m
        , FrameBuffer f w
        , I.Display d f w
        , T.Transport t
        ) => (p -> m d) -> DInputs  ->  m LEDs
leds mkDisplay dinputs = do
    shouldInit <- asks D.shouldInit
    mcu        <- asks D.mcu
    transport  <- asks D.transport
    display    <- mkDisplay $ peripherals mcu
    canvas     <- mkCanvas1D $ I.frameBuffer display "leds"
    order      <- values   "leds_order"       [0, 5, 1, 4, 2, 3]
    t          <- value    "leds_t"           0
    start      <- value    "leds_start"       true
    findMe     <- value    "leds_find_me"     false
    findMeMsg  <- values   "leds_find_me_msg" [actionFindMe, 0]
    colorMsg   <- values   "leds_tx_msg"      [actionRGB]
    pixels     <- records_ "leds_pixels"
    colors     <- records  "leds_colors"      [black, black, black, black, black, black]

    addStruct    (Proxy :: Proxy RGB)
    addStruct    (Proxy :: Proxy HSV)
    addConstArea sinT

    let leds = LEDs { display, canvas, order
                    , t, start, findMe, findMeMsg, colorMsg, shouldInit
                    , pixels, colors
                    , dinputs
                    , transmit = T.transmitBuffer transport
                    }

    addHandler $ I.Render display 30 $ do
        update leds
        render leds

    pure leds

    where black = rgb 0 0 0



update :: LEDs -> Ivory (ProcEffects s ()) ()
update LEDs{..} = do
    start'  <- deref start
    findMe' <- deref findMe
    pixel'  <- local . istruct $ rgb 0 0 0
    pixel'' <- local . istruct $ rgb 1 1 1
    when start'
        (do
            t' <- deref t
            v  <- deref $ addrOf sinT ! toIx t'
            pixel <- local . istruct $ hsv v v v
            hsv'to'rgb pixel pixel'
            when (t' ==? 120 .&& iNot findMe')
                 (store start false)
            store t $ t' + 1
        )
    runDInputs dinputs $ \di ->
        arrayMap $ \ix -> do
            state' <- deref $ addrOf di ! ix ~> state
            jx <- deref $ order ! toIx ix
            ifte_ state'
                (run (pixels ! jx) pixel'')
                (ifte_ start'
                    (run (pixels ! jx) pixel')
                    (run (pixels ! jx) $ colors ! jx)
                )
    where
        run dst src = do
            store (dst ~> r) =<< deref (src ~> r)
            store (dst ~> g) =<< deref (src ~> g)
            store (dst ~> b) =<< deref (src ~> b)



render :: LEDs -> Ivory (ProcEffects s ()) ()
render LEDs{..} = do
    writePixels canvas pixels
    transmitFrameBuffer display $ getBuffer canvas



onFindMe :: KnownNat n
         => LEDs
         -> Buffer n Uint8
         -> Uint8
         -> Ivory (ProcEffects s t) ()
onFindMe LEDs{..} buff size =
    when (size ==? 2) $ do
        v <- unpack buff 1
        pack findMeMsg 1 v
        store findMe v
        store t 0
        store start true
        transmit findMeMsg



onSetColor :: KnownNat n => LEDs -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onSetColor LEDs{..} buff size =
    when (size ==? 5) $ do
        i  <- deref (buff ! 1)
        when (i >=? 1 .&& i <=? 6) $ do
            let ix = toIx $ i - 1
            store (colorMsg ! 1) i
            run ix 2 r
            run ix 3 g
            run ix 4 b
            transmit colorMsg
    where
        run ix jx color = do
            value <- deref $ buff ! toIx jx
            store (colorMsg ! jx) value
            store (colors ! ix ~> color) $ safeCast value / 255



onInitColors :: KnownNat n => LEDs -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onInitColors LEDs{..} buff size =
    when (size ==? 19) $ do
         arrayMap run
         store shouldInit false
    where
        run ix = go ix r 1 >> go ix g 2 >> go ix b 3
        go  ix color offset = do
            value <- deref (buff ! toIx (fromIx ix * 3 + offset))
            store (colors ! ix ~> color) $ safeCast value / 255



sinT :: ConstMemArea (Array 120 (Stored IFloat))
sinT = constArea "sinT" $ iarray $ ival . ifloat . f . fromInteger <$> [0..119]
    where f i = sin (pi * i / 120)
