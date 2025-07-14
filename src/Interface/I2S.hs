{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}

module Interface.I2S where

import           Data.Record
import           Ivory.Language

type SampleStruct = "sample"

type Sample = Record SampleStruct

[ivory|
    struct sample {
        int32_t left;
        int32_t right;
    }
|]
