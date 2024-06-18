{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
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
import           Data.ByteString       (index)
import           Data.Color
import           Data.Display.Canvas1D
import           Data.Matrix
import           Data.Record
import           Data.Serialize
import           Data.Value
import           Endpoint.Dimmers      (brightness)
import           Endpoint.DInputs      as DI
import           Feature.Dimmers       (onOff)
import           Feature.RS485.RBUS    (rbus')
import           Feature.Scd40         (SCD40 (transmit, txBuff))
import           GHC.Arr               (array)
import           GHC.TypeNats
import           Interface.Display     (Display)
import qualified Interface.Display     as I
import           Interface.Flash       as F
import           Interface.MCU
import           Interface.SystemClock (getSystemTime)
import           Ivory.Language
import           Ivory.Language.Proxy
import           Ivory.Stdlib
import           Transport.UDP.RBUS.Tx (lazyTransmit')
import           Util.CRC16



data LEDs pn ln = forall f t. (T.LazyTransport t, Flash f) => LEDs
    { colors     :: Matrix   pn ln Uint32
    , palette    :: Value       (Ix pn)
    , ix         :: Value       (Ix pn)
    , shouldSend :: Value       IBool
    , canvas     :: Canvas1D ln
    , order      :: Values   ln (Ix ln)
    , state      :: Value       IBool
    , brightness :: Value       IFloat
    , pixels     :: Records  ln RGB
    , image      :: Values   ln IBool
    , blink      :: Values   ln IBool
    , blinkPhase :: Value       IBool
    , transport  :: t
    , etc        :: f
    , synced     :: Values   pn IBool
    , synced_    :: Value       IBool
    }



mkLeds :: ( KnownNat pn, KnownNat ln
          , MonadState Context m
          , MonadReader (D.Domain p c) m
          , T.LazyTransport t
          , Flash f
          )
       => Values (Canvas1DSize ln)  Uint8 -> [Ix ln] -> t -> f -> m (LEDs pn ln)
mkLeds frameBuffer order' transport etc = do
    let canvas  = mkCanvas1D frameBuffer
    order      <- values     "leds_order"       order'
    state      <- value      "leds_state"       true
    brightness <- value      "leds_brightness"  0.25
    pixels     <- records_   "leds_pixels"
    image      <- values'    "leds_image"       false
    blink      <- values'    "leds_blink"       false
    blinkPhase <- value      "leds_blink_phase" false
    colors     <- matrix'    "leds_colors"     0
    synced     <- values'    "leds_synced"      true
    synced_    <- value      "leds_synced_"     true
    ix         <- value      "leds_index"       0
    palette    <- value      "leds_palette"     0
    shouldSend <- value      "leds_should_send" true

    addStruct    (Proxy :: Proxy RGB)
    addStruct    (Proxy :: Proxy HSV)

    let leds = LEDs { canvas, colors, palette, ix, shouldSend, order
                    , state, brightness, pixels, image, blink, blinkPhase
                    , transport, etc, synced, synced_
                    }

    addTask $ delay   500 "blink" (store blinkPhase . iNot =<< deref blinkPhase)
    addTask $ delay 1_000 "sync_leds"    $ syncLEDs leds
    addTask $ delay    50 "send_palette" $ sendPalette leds

    addInit "load_leds" $ loadLeds leds

    pure leds



syncLEDs :: (KnownNat pn, KnownNat ln)
         => LEDs pn ln -> Ivory (ProcEffects s t) ()
syncLEDs LEDs{..} = do
    pageOffset <- local $ ival 1024
    arrayMap $ \px -> do
        synced' <- deref $ synced ! px
        pageOffset' <- deref pageOffset
        when (iNot synced') $ do
            erasePage etc pageOffset'
            crc <- local $ istruct initCRC16
            colorOffset <- local $ ival 0
            arrayMap $ \cx -> do
                value <- deref $ colors ! px ! cx
                let r' = castDefault $ (value `iShiftR` 16) .& 0xff
                let g' = castDefault $ (value `iShiftR`  8) .& 0xff
                let b' = castDefault $ value .& 0xff
                updateCRC16 crc r'
                updateCRC16 crc g'
                updateCRC16 crc b'
                colorOffset' <- deref colorOffset
                F.write etc (pageOffset' + colorOffset') value
                store colorOffset $ colorOffset' + 4
            colorOffset' <- deref colorOffset
            let offset = pageOffset' + colorOffset'
            F.write etc offset . safeCast =<< deref (crc ~> msb)
            F.write etc (offset + 4) . safeCast =<< deref (crc ~> lsb)
            store (synced ! px) true
        store pageOffset $ pageOffset' + 1024

    synced_' <- deref synced_
    when (iNot synced_') $ do
        pageOffset' <- deref pageOffset
        erasePage etc pageOffset'
        crc <- local $ istruct initCRC16
        brightness' <-  castDefault . (* 255) <$> deref brightness
        state' <-  safeCast <$> deref state
        updateCRC16 crc brightness'
        updateCRC16 crc state'
        F.write etc pageOffset' $ safeCast brightness'
        F.write etc (pageOffset' + 4) $ safeCast state'
        F.write etc (pageOffset' + 8) . safeCast =<< deref (crc ~> msb)
        F.write etc (pageOffset' + 12) . safeCast =<< deref (crc ~> lsb)
        store synced_ true



loadLeds :: (KnownNat pn, KnownNat ln)
         => LEDs pn ln -> Ivory (ProcEffects s t) ()
loadLeds LEDs{..} = do
    pageOffset <- local $ ival 1024
    arrayMap $ \px -> do
        crc         <- local $ istruct initCRC16
        pageOffset' <- deref pageOffset
        colorOffset <- local $ ival 0
        arrayMap $ \cx -> do
            colorOffset' <- deref colorOffset
            value <- F.read etc $ pageOffset' + colorOffset'
            store (colors ! px ! cx) value
            let r' = castDefault $ (value `iShiftR` 16) .& 0xff
            let g' = castDefault $ (value `iShiftR`  8) .& 0xff
            let b' = castDefault $ value .& 0xff
            updateCRC16 crc r'
            updateCRC16 crc g'
            updateCRC16 crc b'
            store colorOffset $ colorOffset' + 4
        colorOffset' <- deref colorOffset
        let offset = pageOffset' + colorOffset'
        msb' <- F.read etc offset
        lsb' <- F.read etc (offset + 4)
        msb'' <- safeCast <$> deref (crc ~> msb)
        lsb'' <- safeCast <$> deref (crc ~> lsb)
        when (msb' /=? msb'' .|| lsb' /=? lsb'') $ do
            arrayMap $ \cx -> store (colors ! px ! cx) 0x77_77_77
        store pageOffset $ pageOffset' + 1024

    pageOffset' <- deref pageOffset
    crc         <- local $ istruct initCRC16
    brightness' <- castDefault <$> F.read etc pageOffset'
    state'      <- castDefault <$> F.read etc (pageOffset' + 4)
    msb'        <- F.read etc (pageOffset' + 8)
    lsb'        <- F.read etc (pageOffset' + 12)
    updateCRC16 crc brightness'
    updateCRC16 crc state'
    msb''       <- safeCast <$> deref (crc ~> msb)
    lsb''       <- safeCast <$> deref (crc ~> lsb)
    when (msb' ==? msb'' .&& lsb' ==? lsb'') $ do
        store brightness $ safeCast brightness' / 255
        store state $ state' ==? 1




updateLeds :: (KnownNat pn, KnownNat ln) => LEDs pn ln -> Ivory (ProcEffects s ()) ()
updateLeds LEDs{..} = do
    brightness' <- deref brightness
    arrayMap $ \sx -> do
        dx          <- deref $ order ! sx
        state'      <- deref state
        image'      <- deref $ image ! sx
        blink'      <- deref $ blink ! sx
        blinkPhase' <- deref blinkPhase
        palette'    <- deref palette

        ifte_ (state' .&& image' .&& (iNot blink' .|| blink' .&& blinkPhase'))
              (do
                  value  <- deref $ colors ! palette' ! sx
                  let r'  = safeCast $ (value `iShiftR` 16) .& 0xff
                  let g'  = safeCast $ (value `iShiftR`  8) .& 0xff
                  let b'  = safeCast $ value .& 0xff
                  store (pixels ! dx ~> r) $ r' / 255 * brightness'
                  store (pixels ! dx ~> g) $ g' / 255 * brightness'
                  store (pixels ! dx ~> b) $ b' / 255 * brightness'
              )
              (do
                  store (pixels ! dx ~> r) 0
                  store (pixels ! dx ~> g) 0
                  store (pixels ! dx ~> b) 0
              )


render :: (KnownNat ln, KnownNat (Canvas1DSize ln) ) => LEDs pn ln -> Ivory (ProcEffects s ()) ()
render LEDs{..} =
    writePixels canvas pixels



onDo :: (KnownNat n, KnownNat ln)
      => LEDs pn ln -> Buffer n Uint8 -> Uint8
      -> Ivory (ProcEffects s t) ()
onDo LEDs{..} buff size =
    when (size ==? 2) $ do
        lazyTransmit transport 2 $ \transmit -> do
            v <- deref $ buff ! 1
            store state $ v ==? 1
            transmit actionDo
            transmit v
        store synced_ false


onDim :: KnownNat n
      => LEDs pn ln -> Buffer n Uint8 -> Uint8
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
        store synced_ false



onSetColor :: forall n pn ln s t. (KnownNat n, KnownNat pn, KnownNat ln)
           => LEDs pn ln -> Buffer n Uint8 -> Uint8
           -> Ivory (ProcEffects s t) ()
onSetColor LEDs{..} buff size = do
    let ln' = fromIntegral $ fromTypeNat (aNat :: NatType ln)
    let pn' = fromIntegral $ fromTypeNat (aNat :: NatType pn)
    when (size >=? 6 .&& (size - 3) .% 3 ==? 0) $ do
        p  <- deref $ buff ! 1
        i  <- deref $ buff ! 2
        when (p >=?1 .&& p <=? pn' .&& i >=? 1 .&& i <=? ln') $ do
            let p' = toIx $ p - 1
            let i' = i - 1
            let r' = ln' - i'
            let s = (size - 2) `iDiv` 3
            n <- local . ival $ s
            when (r' <? s) $ store n r'
            n' <- deref n
            T.lazyTransmit transport (3 * n' + 3) $ \transmit -> do
                transmit actionRGB
                transmit p
                transmit i
                for (toIx n') $ \ix -> do
                    let dx =  toIx i' + ix
                    r' <- deref $ buff ! toIx (3 * fromIx ix + 3)
                    g' <- deref $ buff ! toIx (3 * fromIx ix + 4)
                    b' <- deref $ buff ! toIx (3 * fromIx ix + 5)
                    let value = (safeCast r' `iShiftL` 16) .| (safeCast g' `iShiftL` 8) .| safeCast b'
                    store (colors ! p' ! dx) value
                    transmit r'
                    transmit g'
                    transmit b'
            store (synced ! p') false



onImage :: forall n pn ln s t. (KnownNat n, KnownNat ln)
        => LEDs pn ln -> Buffer n Uint8 -> Uint8
        -> Ivory (ProcEffects s t) ()
onImage LEDs{..} buff size = do
    let ln' = fromIntegral $ fromTypeNat (aNat :: NatType ln)
    let s' = ln' `iDiv` 8
    n <- local $ ival s'
    let r' = ln' .% 8
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



onBlink :: forall n pn ln s t. (KnownNat n, KnownNat ln)
        => LEDs pn ln -> Buffer n Uint8 -> Uint8
        -> Ivory (ProcEffects s t) ()
onBlink LEDs{..} buff size = do
    let ln' = fromIntegral $ fromTypeNat (aNat :: NatType ln)
    let s' = ln' `iDiv` 8
    n <- local $ ival s'
    let r' = ln' .% 8
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



onPalette :: forall n pn ln s t. (KnownNat n, KnownNat pn, KnownNat ln)
          => LEDs pn ln -> Buffer n Uint8 -> Uint8
          -> Ivory (ProcEffects s t) ()
onPalette LEDs{..} buffer size =
    when (size ==? 2) $ do
        palette' <- unpack buffer 1
        let palettes = fromIntegral $ fromTypeNat (aNat :: NatType pn)
        when (palette' >=? 1 .&& palette' <=? palettes) $ do
            store palette . toIx $ palette' - 1
            lazyTransmit transport 2 $ \transmit -> do
                transmit actionPalette
                transmit palette'



sendLEDs :: KnownNat pn => LEDs pn ln -> Ivory (ProcEffects s t) ()
sendLEDs LEDs{..} = do
    store ix 0
    store shouldSend true
    lazyTransmit transport 2 $ \transmit -> do
        transmit actionDim
        transmit . castDefault . (* 255) =<< deref brightness
    lazyTransmit transport 2 $ \transmit -> do
        transmit actionDo
        transmit . safeCast =<< deref state



sendPalette :: forall pn ln s t. (KnownNat pn, KnownNat ln)
            => LEDs pn ln -> Ivory (ProcEffects s t) ()
sendPalette LEDs{..} = do
    let ln' = fromIntegral $ fromTypeNat (aNat :: NatType ln)
    let pn' = fromIntegral $ fromTypeNat (aNat :: NatType pn)
    shouldSend' <- deref shouldSend
    when shouldSend' $ do
        let n = 3 * ln' + 3
        ix' <- deref ix
        let i' = fromIx ix'
        lazyTransmit transport n $ \transmit -> do
            transmit actionRGB
            transmit . castDefault $ i' + 1
            transmit 1
            arrayMap $ \cx -> do
                value <- deref (colors ! ix' ! cx)
                let r' = castDefault $ (value `iShiftR` 16) .& 0xff
                let g' = castDefault $ (value `iShiftR`  8) .& 0xff
                let b' = castDefault $ value .& 0xff
                transmit r'
                transmit g'
                transmit b'
        ifte_ (i' <? pn' - 1)
              (store ix $ ix' + 1)
              (store shouldSend false)
