module Endpoint.ALED.Animation where

import Core.Context
import Data.Buffer
import Data.Record
import Data.Value
import Endpoint.ALED.Animation.Color.Fade
import Endpoint.ALED.Animation.Color.RandomT
import Endpoint.ALED.Animation.Color.RandomX
import Endpoint.ALED.Animation.Color.SpectrumT
import Endpoint.ALED.Animation.Color.SpectrumX
import Endpoint.ALED.Animation.Data
import Endpoint.ALED.Animation.Mask.Blink
import Endpoint.ALED.Animation.Mask.Eiffel
import Endpoint.ALED.Animation.Mask.FadeOff
import Endpoint.ALED.Animation.Mask.FadeOn
import Endpoint.ALED.Animation.Mask.Off
import Endpoint.ALED.Animation.Mask.On
import Endpoint.ALED.Animation.Mask.Random
import Endpoint.ALED.Animation.Mask.RandomOff
import Endpoint.ALED.Animation.Mask.RandomOn
import Endpoint.ALED.Animation.Mask.Slide
import Endpoint.ALED.Animation.Mask.SlideInOut
import Endpoint.ALED.Animation.Mask.SlideInOut'
import Endpoint.ALED.Animation.Mask.SlideOff
import Endpoint.ALED.Animation.Mask.SlideOffInOut
import Endpoint.ALED.Animation.Mask.SlideOn
import Endpoint.ALED.Animation.Mask.SlideOnInOut
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib
import Util.Random

renderColor ::
    IFloat ->
    Random Uint8 ->
    Record AnimationStruct ->
    Sint32 ->
    Uint16 ->
    Sint32 ->
    Uint8 ->
    Sint32 ->
    IFloat ->
    Ivory (AllowBreak (ProcEffects s ())) IFloat
renderColor brightness random animation segment segmentSize pixel pixelSize subpixel value = do
    animationState' <- deref $ animation ~> animationState
    ifte
        animationState'
        do
            time' <- getTime animation segment
            kind' <- deref $ animation ~> kind
            let (-->) p r = kind' ==? p ==> r animation brightness
            ifte
                (time' <? 0 .|| time' >? 1)
                do
                    pure value
                do
                    cond
                        [ 0x00 --> renderFade time' subpixel value
                        , 0x10 --> renderSpectrumT time' subpixel
                        , 0x11 --> renderSpectrumX time' segmentSize pixel subpixel
                        , 0x20 --> renderRandomT time' subpixel value random
                        , 0x21 --> renderRandomX subpixel value random
                        , true ==> pure value
                        ]
        do
            pure value

renderMask ::
    Random Uint8 ->
    Record AnimationStruct ->
    Sint32 ->
    Uint16 ->
    Sint32 ->
    Ivory (AllowBreak (ProcEffects s ())) IFloat
renderMask random animation segment segmentSize pixel = do
    kind' <- deref $ animation ~> kind
    def <-
        ifte
            (kind' .& 0xf0 ==? 0x00)
            (pure 0)
            (pure 1)
    state <- deref $ animation ~> animationState
    ifte
        state
        do
            time' <- getTime animation segment
            cond
                [ time' <? 0 ==> pure (1 - def)
                , time' >? 1 ==> pure def
                , true ==> do
                    let (-->) p r = kind' ==? p ==> r animation
                    cond
                        [ 0x00 --> renderOff
                        , 0x01 --> renderFadeOff time'
                        , 0x02 --> renderRandomOff time' pixel random
                        , 0x03 --> renderSlideOff time' segmentSize pixel
                        , 0x04 --> renderSlideOffInOut time' segmentSize pixel
                        , 0x10 --> renderOn
                        , 0x11 --> renderFadeOn time'
                        , 0x12 --> renderRandomOn time' pixel random
                        , 0x13 --> renderSlideOn time' segmentSize pixel
                        , 0x14 --> renderSlideOnInOut time' segmentSize pixel
                        , 0x20 --> renderBlink time'
                        , 0x21 --> renderRandom random
                        , 0x22 --> renderEiffel time' random
                        , 0x23 --> renderSlide time' segmentSize pixel
                        , 0x24 --> renderSlideInOut time' segmentSize pixel
                        , 0x25 --> renderSlideInOut' time' segmentSize pixel
                        , true ==> pure 1
                        ]
                ]
        do
            pure def

getTime ::
    Record AnimationStruct ->
    Sint32 ->
    Ivory (AllowBreak (ProcEffects s ())) IFloat
getTime animation segment = do
    inverseDirection' <- deref $ animation ~> inverseDirection
    inLoop' <- deref $ animation ~> inLoop
    phase' <- deref $ animation ~> phase
    time' <- deref $ animation ~> time
    let phase = safeCast segment * phase'
    t <- local . ival $ time' - phase
    when inLoop' do
        t' <- deref t
        store t $ t' - floorF t'
    deref t
