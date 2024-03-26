{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.Smart.Top.LEDs where


import           Control.Monad.Reader  (MonadReader, asks)
import           Control.Monad.State   (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Domain           as D
import           Core.Handler
import qualified Core.Transport        as T
import           Data.Buffer
import           Data.Color
import           Data.Display.Canvas1D
import           Data.Record
import           Data.Serialize
import           Data.Value
import           Endpoint.DInputs      as DI
import           Feature.Scd40         (SCD40 (txBuff))
import           GHC.TypeNats
import           Interface.Display     (Display)
import qualified Interface.Display     as I
import           Interface.MCU
import           Interface.SystemClock (getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib



data LEDs n = forall d f t. (Display d) => LEDs
    { display         :: d
    , canvas          :: Canvas1D n
    , leds'per'button :: Ix       n
    , order           :: Values   n (Ix n)
    , t               :: Value      Sint32
    , start           :: Value      IBool
    , findMe          :: Value      IBool
    , findMeMsg       :: Buffer   n Uint8
    , shouldInit      :: Value      IBool
    , pixels          :: Records  n RGB
    , colors          :: Records  n RGB
    , colorMsg        :: Buffer   5 Uint8
    , dinputs         :: DInputs
    , transmit        :: forall l s t. KnownNat l => Buffer l Uint8 -> Ivory (ProcEffects s t) ()
    }



mkLeds :: ( KnownNat n
          , MonadState Context m
          , MonadReader (D.Domain p c) m
          , I.Display d
          , T.Transport t
          ) => (p -> m d) -> DInputs -> Ix n ->[Ix n] -> t -> m (LEDs n)
mkLeds display' dinputs leds'per'button order' transport = do
    shouldInit <- asks D.shouldInit
    mcu        <- asks D.mcu
    display    <- display' $ peripherals mcu
    canvas     <- mkCanvas1D "leds_canvas"
    order      <- values     "leds_order"       order'
    t          <- value      "leds_t"           0
    start      <- value      "leds_start"       true
    findMe     <- value      "leds_find_me"     false
    findMeMsg  <- values     "leds_find_me_msg" [actionFindMe, 0]
    colorMsg   <- values     "leds_tx_msg"      [actionRGB]
    pixels     <- records_   "leds_pixels"
    colors     <- records    "leds_colors"      [black, black, black, black, black, black]

    addStruct    (Proxy :: Proxy RGB)
    addStruct    (Proxy :: Proxy HSV)
    addConstArea sinT

    let leds = LEDs { display, canvas, leds'per'button, order
                    , t, start, findMe, findMeMsg, colorMsg, shouldInit
                    , pixels, colors
                    , dinputs
                    , transmit = T.transmitBuffer transport
                    }

    addHandler $ I.Render display 30 (runCanvas canvas)
                                     (update leds >> render leds)

    pure leds

    where black = rgb 0 0 0



update :: KnownNat n => LEDs n -> Ivory (ProcEffects s ()) ()
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
            for leds'per'button $ \ kx -> do
                jx <- deref $ order ! (leds'per'button * toIx ix + kx)
                ifte_ state'
                    (run (pixels ! jx) pixel'')
                    (ifte_ start'
                        (run (pixels ! jx) pixel')
                        (run (pixels ! jx) $ colors ! toIx ix)
                    )
    where
        run dst src = do
            store (dst ~> r) =<< deref (src ~> r)
            store (dst ~> g) =<< deref (src ~> g)
            store (dst ~> b) =<< deref (src ~> b)



render :: KnownNat n => LEDs n -> Ivory (ProcEffects s ()) ()
render LEDs{..} =
    writePixels canvas pixels



onFindMe :: (KnownNat l, KnownNat n)
         => LEDs n
         -> Buffer l Uint8
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



onSetColor :: (KnownNat l, KnownNat n) => LEDs n -> Buffer l Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
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



onInitColors :: (KnownNat l, KnownNat n) => LEDs n -> Buffer l Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
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
