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



type SegmentStruct = "segment_struct"

[ivory|
    struct segment_struct
    { segmentSize    :: Uint8
    ; direction      :: IBool
    }
|]



type GroupStruct = "group_struct"

[ivory|
    struct group_struct
    { colors         :: Uint8
    ; pixelSize      :: Uint8
    ; segmentNumber  :: Uint8
    ; brightness     :: IFloat
    ; groupState     :: IBool
    }
|]



data ALED ng ns np = ALED
    { groups     :: Records ng GroupStruct
    , segments   :: Records ns SegmentStruct
    , animations :: Records ng AnimationStruct
    , subPixels  :: Buffer  np Uint8
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

    addConstArea sinT

    groups    <- records' "aled_groups"
                          [ colors         .= ival 0
                          , pixelSize      .= ival 0
                          , segmentNumber  .= ival 0
                          , brightness     .= ival 0.5
                          , groupState     .= ival false
                          ]

    segments  <- records' "aled_segments"
                          [ segmentSize    .= ival 0
                          , direction      .= ival false
                          ]

    animations <- records' "aled_animations"
                          [ kind           .= ival 0
                          , params         .= iarray (ival <$> [0, 0, 0, 0, 0, 0, 0, 0])
                          , time           .= ival 0
                          , dt             .= ival 0
                          , animationState .= ival false
                          , animationLoop  .= ival false
                          , split          .= ival false
                          ]

    subPixels  <- values' "aled_sub_pixels" 0

    pure ALED {groups, segments, animations, subPixels}
