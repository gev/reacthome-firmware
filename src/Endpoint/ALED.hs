{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE QuasiQuotes      #-}

module Endpoint.ALED where

import           Control.Monad.State (MonadState)
import           Core.Context
import           Data.Buffer
import           Data.Record
import           GHC.TypeNats
import           Ivory.Language
import           Prelude             hiding (length)



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
    ; groupIndex    :: Uint8
    ; pixelSize     :: Uint8
    ; segmentNumber :: Uint8
    ; animation     :: Uint8
    ; params        :: Array 8 (Stored Uint8)
    }
|]


data ALED ng ns np = ALED
    { groups    :: Records ng GroupStruct
    , segments  :: Records ns SegmentStruct
    , subPixels :: Buffer  np Uint8
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

    groups   <- records' "aled_groups"
                         [ groupType     .= ival 0
                         , groupIndex    .= ival 0
                         , pixelSize     .= ival 0
                         , segmentNumber .= ival 0
                         , animation     .= ival 0
                         , params        .= iarray (ival <$> [0, 0, 0, 0, 0, 0, 0, 0])
                         ]
    segments <- records' "aled_segments"
                         [ size      .= ival 0
                         , direction .= ival false
                         ]
    subPixels <- buffer "aled_sub_pixels"

    pure ALED {groups, segments, subPixels}
