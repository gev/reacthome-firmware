{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Endpoint.ALED.Animation where

import           Core.Context
import           Data.Buffer
import           Data.Record
import           Data.Value
import           Endpoint.ALED.Animation.Data
import           Endpoint.ALED.Animation.Fade
import           Endpoint.ALED.Animation.RandomT
import           Endpoint.ALED.Animation.RandomX
import           Endpoint.ALED.Animation.SpectrumT
import           Endpoint.ALED.Animation.SpectrumX
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib
import           Util.Random



renderAnimation :: Random Uint8
                -> Record AnimationStruct
                -> Sint32
                -> Uint8
                -> Sint32
                -> Uint8
                -> Sint32
                -> IFloat
                -> Ivory (AllowBreak (ProcEffects s ())) IFloat
renderAnimation random animation segment segmentSize pixel pixelSize subpixel value = do
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
                 ]
         ) $ pure value
