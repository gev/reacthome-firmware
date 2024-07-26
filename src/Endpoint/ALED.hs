{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE QuasiQuotes      #-}

module Endpoint.ALED where

import           Control.Monad.State (MonadState)
import           Core.Context
import           Data.Buffer
import           Data.Record
import           Data.Value
import           GHC.TypeNats
import           Ivory.Language



type SegmentStruct = "segment_struct"

[ivory|
    struct segment_struct
    { size      :: Uint16
    ; direction :: IBool
    }
|]



type GroupStruct = "group_struct"

[ivory|
    struct group_struct
    { groupType     :: Uint8
    ; pixelSize     :: Uint8
    ; segmentNumber :: Uint8
    ; brightness    :: IFloat
    }
|]



type AnimationStruct = "animation_struct"

[ivory|
    struct animation_struct
    { animation     :: Uint8
    ; params        :: Array 8 (Stored Uint8)
    ; time          :: Uint32
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

    groups    <- records' "aled_groups"
                          [ groupType     .= ival 0
                          , pixelSize     .= ival 0
                          , segmentNumber .= ival 0
                          , brightness    .= ival 0.1
                          ]

    segments  <- records' "aled_segments"
                          [ size      .= ival 0
                          , direction .= ival false
                          ]

    animations <- records' "aled_animations"
                          [ animation .= ival 0
                          , params    .= iarray (ival <$> [0, 0, 0, 0, 0, 0, 0, 0])
                          , time      .= ival 0
                          ]
    subPixels  <- values' "aled_sub_pixels" 0

    pure ALED {groups, segments, animations, subPixels}
