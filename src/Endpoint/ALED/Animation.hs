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
            cond [ 0 --> renderFade      subpixel value
                 , 1 --> renderRandomT   subpixel value random
                 , 2 --> renderRandomX   value random
                 , 3 --> renderSpectrumT subpixel
                 , 4 --> renderSpectrumX segmentSize pixel subpixel
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
    animationState' <- deref $ animation ~> animationState
    ifte (animationState' .&& t' >=? 0)
         (do
            renderSlideOn segmentSize pixel t' animation
          --   kind' <- deref $ animation ~> kind
          --   let (-->) p r = kind' ==? p ==> r animation
          --   cond [ 3 --> renderConst
          --        , 0 --> renderRandomOn  pixel random
          --        , 1 --> renderRandomOff pixel random
          --        , 2 --> renderSlideOn  segmentSize pixel
          --        , 4 --> renderSlideOffOut segmentSize pixel
          --        , true ==> pure 1
          --        ]
         ) $ pure 1
