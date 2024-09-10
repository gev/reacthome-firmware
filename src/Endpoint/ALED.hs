{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE QuasiQuotes      #-}

module Endpoint.ALED where

import           Control.Monad.State          (MonadState)
import           Core.Context
import           Data.Buffer
import           Data.Record
import           Data.Value
import           Endpoint.ALED.Animation.Data
import           Endpoint.ALED.Animation.SinT
import           GHC.TypeNats
import           Ivory.Language



type SegmentStruct = "aled_segment_struct"

[ivory|
    struct aled_segment_struct
    { segmentSize    :: Uint8
    ; direction      :: IBool
    }
|]



type GroupStruct = "aled_group_struct"

[ivory|
    struct aled_group_struct
    { colors         :: Uint8
    ; pixelSize      :: Uint8
    ; groupSize      :: Uint16
    ; segmentNumber  :: Uint8
    ; brightness     :: IFloat
    ; groupState     :: IBool
    ; stateChanged   :: IBool
    }
|]


type ClipStruct = "aled_clip_struct"

[ivory|
    struct aled_clip_struct
    { start   :: IFloat
    ; end     :: IFloat
    ; inverse :: IBool
    }
|]



data ALED ng ns np = ALED
    { groups          :: Records ng GroupStruct
    , segments        :: Records ns SegmentStruct
    , colorAnimations :: Records ng AnimationStruct
    , maskAnimations  :: Records ng AnimationStruct
    , clips           :: Records ng ClipStruct
    , subPixels       :: Buffer  np Uint8
    }



mkALED :: ( MonadState Context m
          , KnownNat ng
          , KnownNat ns
          , KnownNat np
          )
       => m (ALED ng ns np)
mkALED = do
    addStruct (Proxy :: Proxy SegmentStruct)
    addStruct (Proxy :: Proxy GroupStruct)
    addStruct (Proxy :: Proxy AnimationStruct)
    addStruct (Proxy :: Proxy ClipStruct)

    addConstArea sinT

    groups          <- records' "aled_groups"
                                [ colors         .= ival 0
                                , pixelSize      .= ival 0
                                , groupSize      .= ival 0
                                , segmentNumber  .= ival 0
                                , brightness     .= ival 0.5
                                , groupState     .= ival false
                                , stateChanged   .= ival true
                                ]

    segments        <- records' "aled_segments"
                                [ segmentSize    .= ival 0
                                , direction      .= ival false
                                ]

    colorAnimations <- records' "aled_color_animations"
                                [ kind           .= ival 0
                                , params         .= iarray (ival <$> replicate 8 0)
                                , time           .= ival 0
                                , dt             .= ival 0
                                , phase          .= ival 0
                                , split          .= ival false
                                , animationState .= ival false
                                , animationLoop  .= ival false
                                ]

    maskAnimations  <- records' "aled_mask_animations"
                                [ kind           .= ival 0
                                , params         .= iarray (ival <$> replicate 8 0)
                                , time           .= ival 0
                                , dt             .= ival 0
                                , phase          .= ival 0
                                , split          .= ival false
                                , animationState .= ival false
                                , animationLoop  .= ival false
                                ]

    clips           <- records' "aled_clips"
                                [ start   .= ival 0
                                , end     .= ival 1
                                , inverse .= ival false
                                ]

    subPixels  <- values' "aled_sub_pixels" 0

    pure ALED {groups, segments, colorAnimations, maskAnimations, clips, subPixels}
