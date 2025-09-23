{-# LANGUAGE QuasiQuotes #-}

module Interface.I2S where

import Data.Record
import Ivory.Language

type SampleStruct = "sample"

type Sample = Record SampleStruct

[ivory|
    struct sample {
        int32_t left;
        int32_t right;
    }
|]

infix 4 <==
(<==) :: Sample -> Sample -> Ivory eff ()
(<==) dst src = do
    store (dst ~> left) =<< deref (src ~> left)
    store (dst ~> right) =<< deref (src ~> right)
