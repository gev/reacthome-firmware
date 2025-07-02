{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DataKinds #-}

module Interface.I2S where

import Ivory.Language
import Data.Record

type SampleStruct = "sample"

type Sample = Record SampleStruct

[ivory|
    struct sample {
        uint32_t left;
        uint32_t right;
    }
|]