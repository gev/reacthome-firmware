module Feature.Mix.Indicator where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Context
import Core.Domain as D
import Core.Handler
import qualified Core.Transport as T
import Data.Buffer
import Data.Color
import Data.Display.Canvas1D
import Data.Record
import Data.Serialize
import Data.Value
import Endpoint.ATS
import Endpoint.DInputs as DI
import Endpoint.Relays as R
import GHC.TypeNats
import Interface.Display (Display, Render (Render))
import Interface.MCU
import Interface.SystemClock (getSystemTime)
import Ivory.Language
import Ivory.Stdlib

data Indicator ni no = forall d. (Display d) => Indicator
    { display :: d
    , hue :: IFloat
    , ats :: ATS
    , dinputs :: DInputs ni
    , relays :: Relays no
    , t :: Value Sint32
    , dt :: Value Sint32
    , phi :: Value Sint32
    , dphi :: Value Sint32
    , start :: Value IBool
    , findMe :: Value IBool
    , findMeMsg :: Buffer 2 Uint8
    , canvas :: Canvas1D 20
    , pixels :: Records 20 RGB
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
    , Handler (Render 60) d
    , T.Transport t
    , KnownNat ni
    , KnownNat no
    ) =>
    (p -> m d) ->
    IFloat ->
    ATS ->
    DInputs ni ->
    Relays no ->
    t ->
    m (Indicator ni no)
indicator mkDisplay hue ats dinputs relays transport = do
    mcu <- asks D.mcu
    display <- mkDisplay $ peripherals mcu
    frameBuffer <- values' "indicator_frame_buffer" 0
    let canvas = mkCanvas1D frameBuffer
    t <- value "indicator_t" 0
    dt <- value "indicator_dt" 1
    phi <- value "indicator_phi" 0
    dphi <- value "indicator_dphi" 1
    start <- value "indicator_start" true
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
                , hue
                , t
                , dt
                , phi
                , dphi
                , start
                , findMe
                , findMeMsg
                , pixels
                , ats
                , dinputs
                , relays
                , transmit = T.transmitBuffer transport
                }

    addHandler $ Render display 25 frameBuffer do
        update indicator
        render indicator
        pure true

    pure indicator

update ::
    (KnownNat ni, KnownNat no) =>
    Indicator ni no ->
    Ivory (ProcEffects s ()) ()
update Indicator{..} = do
    phi' <- deref phi
    pixel <- local . istruct $ hsv hue 1 maxValue
    start' <- deref start

    arrayMap \ix -> do
        let i = fromIx ix
        let x = toIx (10 * i + phi')
        sin' <- deref $ addrOf sinT ! x
        y <- assign $ maxValue * (0.2 + 0.8 * sin')

        ifte_
            start'
            do
                let v' = safeCast phi' / 100
                store (pixel ~> s) v'
                store (pixel ~> v) $ y * v'
                when (phi' ==? 100) $ store start false
            do
                store (pixel ~> v) y

        renderPixel pixel i ats dinputs relays
        hsv'to'rgb pixel $ pixels ! ix

    findMe' <- deref findMe
    when findMe' do
        t' <- deref t
        store (pixel ~> v) 1
        store (pixel ~> s) 0.5
        hsv'to'rgb pixel $ pixels ! toIx (9 - t')
        hsv'to'rgb pixel $ pixels ! toIx (10 + t')
        cond_
            [ t' ==? 0 ==> store dt 1
            , t' ==? 9 ==> store dt (-1)
            ]
        dt' <- deref dt
        store t (t' + dt')
    cond_
        [ phi' ==? 0 ==> store dphi 1
        , phi' ==? 200 ==> store dphi (-1)
        ]
    dphi' <- deref dphi
    store phi (phi' + dphi')

renderPixel ::
    (KnownNat ni, KnownNat no) =>
    Ref s (Struct HSV) ->
    Sint32 ->
    ATS ->
    DInputs ni ->
    Relays no ->
    Ivory eff ()
renderPixel pixel i ATS{..} DInputs{dinputs} Relays{relays} = do
    mode' <- deref mode
    cond_
        [ mode'
            ==? mode_N1_G
            ==> cond_
                [ i >=? 13 .&& i <=? 16 ==> renderLine 1 (dinputs ! 1) (relays ! 0)
                , i >=? 3 .&& i <=? 6 ==> renderGenerator 2 (dinputs ! 10) (relays ! 4) (relays ! 5)
                , true ==> store (pixel ~> v) 0
                ]
        , mode'
            ==? mode_N2
            ==> cond_
                [ i >=? 13 .&& i <=? 16 ==> renderLine 1 (dinputs ! 1) (relays ! 0)
                , i >=? 3 .&& i <=? 6 ==> renderLine 2 (dinputs ! 3) (relays ! 1)
                , true ==> store (pixel ~> v) 0
                ]
        , mode'
            ==? mode_N2_G
            ==> cond_
                [ i >=? 15 .&& i <=? 18 ==> renderLine 1 (dinputs ! 1) (relays ! 0)
                , i >=? 8 .&& i <=? 11 ==> renderLine 2 (dinputs ! 3) (relays ! 1)
                , i >=? 1 .&& i <=? 4 ==> renderGenerator 3 (dinputs ! 10) (relays ! 4) (relays ! 5)
                , true ==> store (pixel ~> v) 0
                ]
        ]
  where
    renderLine i hasVoltage relay = do
        let h' = pixel ~> h
        hasVoltage' <- deref $ hasVoltage ~> DI.state
        lineError' <- deref $ error ! toIx i
        when (lineError' /=? errorNone) do
            t <- getSystemTime clock
            let t' = t ./ 500
            when (t' .% 2 ==? 0) do
                store (pixel ~> v) 0
        ifte_
            hasVoltage'
            do
                source' <- deref source
                isOn <- deref $ relay ~> R.state
                ifte_
                    (isOn .&& source' ==? i)
                    do store h' 120
                    do store h' 60
            do
                store h' 0

    renderGenerator i hasVoltage relay start = do
        let h' = pixel ~> h
        hasVoltage' <- deref $ hasVoltage ~> DI.state
        generatorError <- deref $ error ! 0
        lineError' <- deref $ error ! toIx i
        when (lineError' /=? errorNone .|| generatorError /=? errorNone) do
            t <- getSystemTime clock
            let t' = t ./ 500
            when (t' .% 2 ==? 0) do
                store (pixel ~> v) 0
        ifte_
            hasVoltage'
            do
                source' <- deref source
                isOn <- deref $ relay ~> R.state
                ifte_
                    (isOn .&& source' ==? i)
                    do store h' 120
                    do store h' 60
            do
                isOn <- deref $ start ~> R.state
                ifte_
                    (isOn .|| generatorError /=? errorNone)
                    do store h' 240
                    do store h' 0

render :: Indicator ni no -> Ivory (ProcEffects s ()) IBool
render Indicator{..} =
    writePixels canvas pixels

onFindMe :: (KnownNat l) => Indicator ni no -> Buffer l Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onFindMe Indicator{..} buff size =
    when (size >=? 2) do
        v <- unpack buff 1
        pack findMeMsg 1 v
        store findMe v
        transmit findMeMsg

sinT :: ConstMemArea (Array 200 (Stored IFloat))
sinT = constArea "indicator_sin_table" $ iarray $ ival . ifloat . f . fromIntegral <$> [-50 .. 149]
  where
    f i = (1 + sin (pi * i / 100)) / 2
