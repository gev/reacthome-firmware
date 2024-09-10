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
import           Endpoint.ALED.Animation.Mask.Random
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
            time' <- getTime animation segment
            kind' <- deref $ animation ~> kind
            let (-->) p r = kind' ==? p ==> r animation
            ifte (time' <? 0 .|| time' >? 1)
                 (pure value)
                 (cond [ 0x00 --> renderFade      time' subpixel value
                       , 0x10 --> renderSpectrumT time' subpixel
                       , 0x11 --> renderSpectrumX time' segmentSize pixel subpixel
                       , 0x20 --> renderRandomT   time' subpixel value random
                       , 0x21 --> renderRandomX   value random
                       , true ==> pure value
                       ]
                 )
         ) $ pure value


renderMask :: Random Uint8
            -> Record AnimationStruct
            -> Sint32
            -> Uint16
            -> Sint32
            -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderMask random animation segment segmentSize pixel = do
     kind' <- deref $ animation ~> kind
     animationState' <- deref $ animation ~> animationState
     ifte animationState'
          (do
              time' <- getTime animation segment
              cond [ time' <? 0 ==> before kind'
                   , time' >? 1 ==> after  kind'
                   , true ==> do
                        let (-->) p r = kind' ==? p ==> r animation
                        cond [ 0x01 --> renderRandomOff   time' pixel random
                             , 0x02 --> renderSlideOff    time' segmentSize pixel
                             , 0x03 --> renderSlideOff'   time' segmentSize pixel
                             , 0x04 --> renderSlideOffIn  time' segmentSize pixel
                             , 0x05 --> renderSlideOffOut time' segmentSize pixel

                             , 0x11 --> renderRandomOn    time' pixel random
                             , 0x12 --> renderSlideOn     time' segmentSize pixel
                             , 0x13 --> renderSlideOn'    time' segmentSize pixel
                             , 0x14 --> renderSlideOnIn   time' segmentSize pixel
                             , 0x15 --> renderSlideOnOut  time' segmentSize pixel

                             , 0x21 --> renderRandom      random
                             , 0x22 --> renderEiffel      time' random
                             , 0x23 --> renderSlide       time' segmentSize pixel
                             , 0x24 --> renderSlide'      time' segmentSize pixel
                             , 0x25 --> renderSlide''     time' segmentSize pixel
                             , 0xff --> renderConst
                             , true ==> pure 1
                             ]
                   ]
          ) $ after kind'

before kind =
      ifte (kind .& 0xf0 ==? 0x00)
           (pure 1)
           (pure 0)

after kind =
      ifte (kind .& 0xf0 ==? 0x00)
           (pure 0)
           (pure 1)

getTime animation segment = do
      inLoop'  <- deref $ animation ~> inLoop
      phase'   <- deref $ animation ~> phase
      time'    <- deref $ animation ~> time
      let phase = safeCast segment * phase'
      t <- local . ival $ time' - phase
      when inLoop' $ do
            t' <- deref t
            cond_ [ t' <? 0 ==> store t (t' - floorF t')
                  , t' >? 1 ==> store t (ceilF t' - t')
                  ]
      deref t
