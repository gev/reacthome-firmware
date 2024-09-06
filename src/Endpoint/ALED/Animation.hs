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
import           Endpoint.ALED.Animation.Mask.RandomOff
import           Endpoint.ALED.Animation.Mask.RandomOn
import           Endpoint.ALED.Animation.Mask.Slide
import           Endpoint.ALED.Animation.Mask.Slide'
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
            -> Uint32
            -> Sint32
            -> Uint8
            -> Sint32
            -> Uint8
            -> Sint32
            -> IFloat
            -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderColor random animation frame segment segmentSize pixel pixelSize subpixel value = do
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
            -> Uint32
            -> Sint32
            -> Uint8
            -> Sint32
            -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderMask random animation frame segment segmentSize pixel = do
    animationState' <- deref $ animation ~> animationState
    ifte animationState'
         (do
            renderSlide segmentSize pixel animation
          --   kind' <- deref $ animation ~> kind
          --   let (-->) p r = kind' ==? p ==> r animation
          --   cond [ 3 --> renderConst
          --        , 4 --> renderRandomOn  frame pixel random
          --        , 2 --> renderRandomOff frame pixel random
          --        , 0 --> renderSlideOn  segmentSize pixel
          --        , 1 --> renderSlideOffOut segmentSize pixel
          --        , true ==> pure 1
          --        ]
         ) $ pure 1
