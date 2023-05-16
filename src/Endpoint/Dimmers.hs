{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes  #-}

module Endpoint.Dimmers where

import           Data.Buffer
import           Data.Record
import           Ivory.Language


type DimmerStruct = "dimmer_struct"

[ivory|
    struct dimmer_struct
    { group    :: Uint8
    ; mode     :: Uint8
    ; value    :: Uint8
    ; velocity :: Uint8
    }
|]

data Groups = Groups
    { runDimmers :: RunRecords DimmerStruct
    , payload    :: Buffer 7 Uint8
    }
