{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Util.Version where

import           Ivory.Language

type Version = "version_struct"

[ivory|
    struct version_struct {
        major :: Uint8;
        minor :: Uint8;
    }
|]

initVersion :: [InitStruct Version]
initVersion =  [ major .= ival 0
               , minor .= ival 0
               ]