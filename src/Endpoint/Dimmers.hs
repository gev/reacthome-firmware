{-# LANGUAGE DataKinds #-}

module Endpoint.Dimmers where

type GroupStruct = "dimmer_struct"

[ivory|
    struct dimmer_struct
    { group         :: Uint8
    ; type          :: Uint8
    ; value         :: Uint8
    ; velocity      :: Uint8
    }
|]