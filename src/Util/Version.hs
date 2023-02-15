{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}

module Util.Version
    ( Version
    , version
    , getVersion
    , major
    , minor
    ) where

import           Include
import           Ivory.Language
import           Util.Data.Class
import           Util.Data.Record



type VersionStruct = "version_struct"


newtype Version = Version {getVersion :: Record VersionStruct}


[ivory|
    struct version_struct {
        major :: Uint8;
        minor :: Uint8;
    }
|]



version :: String -> Uint8 -> Uint8 -> Version
version name maj min = Version $ record name [ major .= ival maj
                                             , minor .= ival min
                                             ]



instance Include Version where
    include (Version v) = do
        defStruct (Proxy :: Proxy VersionStruct)
        include v
