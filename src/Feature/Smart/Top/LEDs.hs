{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}



module Feature.Smart.Top.LEDs where


import           Control.Monad.Reader  (MonadReader, asks, forM_, zipWithM_)
import           Control.Monad.State   (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Domain           as D
import           Core.FSM              (transit)
import           Core.Handler
import           Core.Task             (delay)
import           Core.Transport        (LazyTransport (lazyTransmit))
import qualified Core.Transport        as T
import           Data.Buffer
import           Data.Color
import           Data.Display.Canvas1D
import           Data.Record
import           Data.Serialize
import           Data.Value
import           Endpoint.Dimmers      (brightness)
import           Endpoint.DInputs      as DI
import           Feature.Scd40         (SCD40 (transmit, txBuff))
import           GHC.TypeNats
import           Implementation.Blink  (blink)
import           Interface.Display     (Display)
import qualified Interface.Display     as I
import           Interface.MCU
import           Interface.SystemClock (getSystemTime)
import           Ivory.Language
import           Ivory.Language.Proxy
import           Ivory.Stdlib



data LEDs (l :: Nat) = forall f t. T.LazyTransport t => LEDs
    { canvas     :: Canvas1D l
    , order      :: Values   l (Ix l)
    , state      :: Value      IBool
    , brightness :: Value      IFloat
    , pixels     :: Records  l RGB
    , colors     :: Records  l RGB
    , image      :: Values   l IBool
    , blink      :: Values   l IBool
    , blinkPhase :: Value      IBool
    , transport  :: t
    }



mkLeds :: forall l m p c t.
          ( KnownNat l
          , MonadState Context m
          , MonadReader (D.Domain p c) m
          , T.LazyTransport t
          )
       => Values (Canvas1DSize l)  Uint8 -> [Ix l] -> t -> m (LEDs l)
mkLeds frameBuffer order' transport = do
    let l' = fromInteger $ fromTypeNat (aNat :: NatType l)
    let canvas  = mkCanvas1D frameBuffer
    order      <- values     "leds_order"       order'
    state      <- value      "leds_state"       true
    brightness <- value      "leds_brightness"  1
    pixels     <- records_   "leds_pixels"
    colors     <- records    "leds_colors"      $ replicate l' black
    image      <- values     "leds_image"       $ replicate l' true
    blink      <- values     "leds_blink"       $ replicate l' false
    blinkPhase <- value      "leds_blink_phase" false

    addStruct    (Proxy :: Proxy RGB)
    addStruct    (Proxy :: Proxy HSV)


    addTask $ delay 500 "blink" (store blinkPhase . iNot =<< deref blinkPhase)

    pure LEDs { canvas, order
              , state, brightness, pixels, colors, image, blink, blinkPhase
              , transport
              }

    where black = rgb 0 0 0



updateLeds :: KnownNat l => LEDs l -> Ivory (ProcEffects s ()) ()
updateLeds LEDs{..} = do
    brightness' <- deref brightness
    arrayMap $ \sx -> do
        dx <- deref $ order ! sx
        state' <- deref state
        image'  <- deref $ image ! sx
        blink'  <- deref $ blink ! sx
        blinkPhase' <- deref blinkPhase
        forM_ [r, g, b] $ \c ->
            ifte_ (state' .&& image' .&& (iNot blink' .|| blink' .&& blinkPhase'))
                  (store (pixels ! dx ~> c) . (* brightness') =<< deref (colors ! toIx sx ~> c))
                  (store (pixels ! dx ~> c) 0)



render :: (KnownNat l, KnownNat (Canvas1DSize l) ) => LEDs l -> Ivory (ProcEffects s ()) ()
render LEDs{..} =
    writePixels canvas pixels



onDo :: (KnownNat n, KnownNat l)
      => LEDs l
      -> Buffer n Uint8
      -> Uint8
      -> Ivory (ProcEffects s t) ()
onDo LEDs{..} buff size =
    when (size ==? 2) $
        lazyTransmit transport 2 $ \transmit -> do
            v <- deref $ buff ! 1
            store state $ v ==? 1
            transmit actionDo
            transmit v



onDim :: KnownNat n
      => LEDs l
      -> Buffer n Uint8
      -> Uint8
      -> Ivory (ProcEffects s t) ()
onDim LEDs{..} buff size =
    when (size ==? 2) $ do
        brightness' <- deref $ buff ! 1
        ifte_ (brightness' ==? 0)
              (do
                    store state false
                    lazyTransmit transport 2 $ \transmit -> do
                        transmit actionDo
                        transmit 0
                    store brightness $ 1 / 255
                    lazyTransmit transport 2 $ \transmit -> do
                        transmit actionDim
                        transmit 1
              )
              (do
                    store brightness $ safeCast brightness' / 255
                    lazyTransmit transport 2 $ \transmit -> do
                        transmit actionDim
                        transmit brightness'
              )



onSetColor :: forall n l s t. (KnownNat n, KnownNat l)
           => LEDs l -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onSetColor LEDs{..} buff size = do
    let l' = fromInteger $ fromTypeNat (aNat :: NatType l)
    when ((size - 2) .% 3 ==? 0) $ do
        i  <- deref (buff ! 1)
        when (i >=? 1 .&& i <=? l') $ do
            let i' = i - 1
            let r' = l' - i'
            let s = (size - 2) `iDiv` 3
            n <- local . ival $ s
            when (r' <? s) $ store n r'
            n' <- deref n
            T.lazyTransmit transport (3 * n' + 2) $ \transmit -> do
                transmit actionRGB
                transmit i
                for (toIx n') $ \ix -> do
                    let setColor color offset = do
                            let sx = toIx (3 * fromIx ix + offset)
                            value <- deref $ buff ! sx
                            let dx =  toIx i' + ix
                            store (colors ! dx ~> color) $ safeCast value / 255
                            transmit value
                    zipWithM_ setColor [r, g, b] [2, 3, 4]



onImage :: forall n l s t. (KnownNat n, KnownNat l)
        => LEDs l
        -> Buffer n Uint8
        -> Uint8
        -> Ivory (ProcEffects s t) ()
onImage LEDs{..} buff size = do
    let l' = fromInteger $ fromTypeNat (aNat :: NatType l)
    let s' = l' `iDiv` 8
    n <- local $ ival s'
    let r' = l' .% 8
    when (r' >? 0) $ store n (s' + 1)
    n' <- deref n
    when ((size - 1) ==? n') $ do
        v <- local $ ival 0
        T.lazyTransmit transport (n' + 1) $ \transmit -> do
            transmit actionImage
            arrayMap $ \dx -> do
                let d = fromIx dx
                when (d .% 8 ==? 0) $ do
                    let sx = toIx $ 1 + d `iDiv` 8
                    store v =<< deref (buff ! sx)
                    transmit =<< deref v
                v' <- deref v
                let image' = v' .& 1 ==? 1
                store (image ! dx) image'
                store v $ v' `iShiftR` 1



onBlink :: forall n l s t. (KnownNat n, KnownNat l)
        => LEDs l
        -> Buffer n Uint8
        -> Uint8
        -> Ivory (ProcEffects s t) ()
onBlink LEDs{..} buff size = do
    let l' = fromInteger $ fromTypeNat (aNat :: NatType l)
    let s' = l' `iDiv` 8
    n <- local $ ival s'
    let r' = l' .% 8
    when (r' >? 0) $ store n (s' + 1)
    n' <- deref n
    when ((size - 1) ==? n') $ do
        v <- local $ ival 0
        T.lazyTransmit transport (n' + 1) $ \transmit -> do
            transmit actionBlink
            arrayMap $ \dx -> do
                let d = fromIx dx
                when (d .% 8 ==? 0) $ do
                    let sx = toIx $ 1 + d `iDiv` 8
                    store v =<< deref (buff ! sx)
                    transmit =<< deref v
                v' <- deref v
                let blink' = v' .& 1 ==? 1
                store (blink ! dx) blink'
                store v $ v' `iShiftR` 1


onInitColors :: forall n l s t. (KnownNat n, KnownNat l)
             => LEDs l -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) IBool
onInitColors LEDs{..} buff size = do
    let l' = fromInteger $ fromTypeNat (aNat :: NatType l)
    let s' = l' `iDiv` 8
    n <- local $ ival s'
    let r' = l' .% 8
    when (r' >? 0) $ store n (s' + 1)
    n' <- deref n
    ifte (size ==? l' * 3 + 2 * n' + 4)
        (do
            store state . (==? 1) =<< deref (buff ! 2)
            store brightness . (/ 255) . safeCast =<< deref (buff ! 3)
            v <- local $ ival 0
            arrayMap $ \dx -> do
                let d = fromIx dx
                when (d .% 8 ==? 0) $ do
                    let sx = toIx $ 4 + (d `iDiv` 8)
                    store v =<< deref (buff ! sx)
                v' <- deref v
                let image' = v' .& 1 ==? 1
                store (image ! dx) image'
                store v $ v' `iShiftR` 1
            arrayMap $ \dx -> do
                let d = fromIx dx
                when (d .% 8 ==? 0) $ do
                    let sx = toIx $ 4 + safeCast n' + (d `iDiv` 8)
                    store v =<< deref (buff ! sx)
                v' <- deref v
                let blink' = v' .& 1 ==? 1
                store (blink ! dx) blink'
                store v $ v' `iShiftR` 1
            arrayMap run
            pure true
        )
        (  pure false
        )
        where
            run ix = go ix r 20 >> go ix g 21 >> go ix b 22
            go  ix color offset = do
                value <- deref (buff ! toIx (fromIx ix * 3 + offset))
                store (colors ! ix ~> color) $ safeCast value / 255
