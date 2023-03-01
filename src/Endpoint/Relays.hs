{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}

module Endpoint.Relays where

import           Ivory.Language


type RelayStruct = "relay_struct"

[ivory|
    struct relay_struct
    { state     :: IBool
    ; delay     :: Uint32
    ; timestamp :: Uint32
    }
|]
