{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}

module Interface.I2S where

import           Data.Record
import           Ivory.Language

type SampleStruct = "sample"

type Sample = Record SampleStruct

[ivory|
    struct sample {
        uint32_t left;
        uint32_t right;
    }
|]
