{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Endpoint.ALED.Animation where

import           Core.Context
import           Data.Buffer
import           Data.Record
import           Data.Value
import           Endpoint.ALED.Animation.Color.Fade
import           Endpoint.ALED.Animation.Color.RandomT
import           Endpoint.ALED.Animation.Color.RandomX
import           Endpoint.ALED.Animation.Color.SpectrumT
import           Endpoint.ALED.Animation.Color.SpectrumX
import           Endpoint.ALED.Animation.Data
import           Endpoint.ALED.Animation.Mask.Const
import           Endpoint.ALED.Animation.Mask.Eiffel
import           Endpoint.ALED.Animation.Mask.Random      (renderRandom)
import           Endpoint.ALED.Animation.Mask.RandomOff
import           Endpoint.ALED.Animation.Mask.RandomOn
import           Endpoint.ALED.Animation.Mask.Slide
import           Endpoint.ALED.Animation.Mask.Slide'
import           Endpoint.ALED.Animation.Mask.Slide''
import           Endpoint.ALED.Animation.Mask.SlideOff
import           Endpoint.ALED.Animation.Mask.SlideOff'
import           Endpoint.ALED.Animation.Mask.SlideOffIn
import           Endpoint.ALED.Animation.Mask.SlideOffOut
import           Endpoint.ALED.Animation.Mask.SlideOn
import           Endpoint.ALED.Animation.Mask.SlideOn'
import           Endpoint.ALED.Animation.Mask.SlideOnIn
import           Endpoint.ALED.Animation.Mask.SlideOnOut
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib
import           Util.Random



renderColor :: Random Uint8
            -> Record AnimationStruct
            -> Sint32
            -> Uint16
            -> Sint32
            -> Uint8
            -> Sint32
            -> IFloat
            -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderColor random animation segment segmentSize pixel pixelSize subpixel value = do
    animationState' <- deref $ animation ~> animationState
    ifte animationState'
         (do
            kind' <- deref $ animation ~> kind
            let (-->) p r = kind' ==? p ==> r animation
            cond [ 0x00 --> renderFade      subpixel value
                 , 0x10 --> renderSpectrumT subpixel
                 , 0x11 --> renderSpectrumX segmentSize pixel subpixel
                 , 0x30 --> renderRandomT   subpixel value random
                 , 0x31 --> renderRandomX   value random
                 , true ==> pure value
                 ]
         ) $ pure value


renderMask :: Random Uint8
            -> Record AnimationStruct
            -> Sint32
            -> Uint16
            -> Sint32
            -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderMask random animation segment segmentSize pixel = do
    t <- local $ ival 0
    time' <- deref $ animation ~> time
    phase' <- deref $ animation ~> phase
    let phase = safeCast segment * phase'
    ifte_ (time' <? 0)
          (store t $ time' - phase)
          (do
               let t' = time' - phase
               ifte_ (t' <? 0)
                     (store t $ t' - floorF t')
                     (store t t')
          )
    t' <- deref t
    kind' <- deref $ animation ~> kind
    animationState' <- deref $ animation ~> animationState
    ifte (animationState' .&& t' >=? 0)
         (do
            let (-->) p r = kind' ==? p ==> r animation
            cond [ 0x01 --> renderRandomOff   pixel random
                 , 0x02 --> renderSlideOff    segmentSize pixel
                 , 0x03 --> renderSlideOff'   segmentSize pixel
                 , 0x04 --> renderSlideOffIn  segmentSize pixel
                 , 0x05 --> renderSlideOffOut segmentSize pixel

                 , 0x11 --> renderRandomOn    pixel random
                 , 0x12 --> renderSlideOn     segmentSize pixel t'
                 , 0x13 --> renderSlideOn'    segmentSize pixel
                 , 0x14 --> renderSlideOnIn   segmentSize pixel
                 , 0x15 --> renderSlideOnOut  segmentSize pixel

                 , 0x20 --> renderConst
                 , 0x21 --> renderRandom      random
                 , 0x22 --> renderEiffel      random
                 , 0x23 --> renderSlide       segmentSize pixel
                 , 0x24 --> renderSlide'      segmentSize pixel
                 , 0x25 --> renderSlide''     segmentSize pixel

                 , true ==> pure 1
                 ]
         ) $ ifte (kind' .& 0x10 ==? 0x00)
                  (pure 0)
                  (pure 1)
