module Feature.IndicatorFlush where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Context
import Core.Domain as D
import Core.Handler
import Core.Meta
import Core.Transport qualified as T
import Data.Buffer
import Data.Color
import Data.Display.Canvas1D
import Data.Record
import Data.Serialize
import Data.Value
import GHC.TypeNats
import Interface.Display (Display, Render (Render))
import Interface.MCU
import Interface.MCU qualified as I
import Ivory.Language
import Ivory.Stdlib

data IndicatorFlush n = forall d. (Display d) => Indicator
    { display :: d
    , t :: Value Sint32
    , findMe :: Value IBool
    , findMeMsg :: Buffer 2 Uint8
    , canvas :: Canvas1D n
    , pixels :: Records n RGB
    , transmit ::
        forall l.
        (KnownNat l) =>
        Buffer l Uint8 ->
        forall s t.
        Ivory (ProcEffects s t) ()
    }

maxValue = 0.3 :: IFloat

indicator ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , Display d
    , Handler (Render (Canvas1DSize n)) d
    , KnownNat n
    , KnownNat (Canvas1DSize n)
    , T.Transport t
    ) =>
    (p -> m d) ->
    t ->
    m (IndicatorFlush n)
indicator mkDisplay transport = do
    meta <- asks D.meta
    platform <- I.platform meta.mcu
    display <- mkDisplay platform.peripherals
    frameBuffer <- values' "indicator_frame_buffer" 0
    let canvas = mkCanvas1D frameBuffer
    t <- value "indicator_t" 0
    findMe <- value "indicator_find_me" false
    findMeMsg <- values "indicator_find_me_msg" [0xfa, 0]
    pixels <- records_ "indicator_pixels"

    addStruct (Proxy :: Proxy RGB)
    addStruct (Proxy :: Proxy HSV)
    addConstArea sinT

    let indicator =
            Indicator
                { display
                , canvas
                , t
                , findMe
                , findMeMsg
                , pixels
                , transmit = T.transmitBuffer transport
                }

    addHandler $ Render display 25 frameBuffer do
        update indicator
        render indicator
        pure true

    pure indicator

update ::
    (KnownNat n) =>
    IndicatorFlush n ->
    Ivory (ProcEffects s ()) ()
update Indicator{..} = do
    pixel <- local . istruct $ rgb 0 0 0
    findMe' <- deref findMe
    when
        findMe'
        do
            t' <- deref t
            v <- deref $ addrOf sinT ! toIx t'
            pixel' <- local . istruct $ hsv v v v
            hsv'to'rgb pixel' pixel
            store t $ t' + 1
    arrayMap \ix ->
        run (pixels ! ix) pixel
  where
    run dst src = do
        store (dst ~> r) =<< deref (src ~> r)
        store (dst ~> g) =<< deref (src ~> g)
        store (dst ~> b) =<< deref (src ~> b)

render ::
    (KnownNat n, KnownNat (Canvas1DSize n)) =>
    IndicatorFlush n ->
    Ivory (ProcEffects s ()) IBool
render Indicator{..} =
    writePixels canvas pixels

onFindMe ::
    (KnownNat l) =>
    IndicatorFlush n ->
    Buffer l Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onFindMe Indicator{..} buff size =
    when (size >=? 2) do
        v <- unpack buff 1
        pack findMeMsg 1 v
        store findMe v
        when v do
            store t 0
        transmit findMeMsg

sinT :: ConstMemArea (Array 120 (Stored IFloat))
sinT = constArea "indicator_sin_table" $ iarray $ ival . ifloat . f . fromIntegral <$> [0 .. 119]
  where
    f i = sin (pi * i / 120)
