{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}

module Util.Version
    ( Version
    , version
    , major
    , minor
    ) where

import           Include
import           Ivory.Language
import           Util.Data.Class
import           Util.Data.Record



type Version = Version' VersionStruct


type VersionStruct = "version_struct"


newtype Version' v = Version {getVersion :: Record v}


[ivory|
    struct version_struct {
        major :: Uint8;
        minor :: Uint8;
    }
|]



version :: Uint8 -> Uint8 -> Version
version maj min = Version $ record "version" [ major .= ival maj
                                             , minor .= ival min
                                             ]



instance Include (Version' VersionStruct) where
    include = include . getVersion


instance Rec Version' VersionStruct where
    getRecord = getRecord . getVersion
