{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}

module Core.Version
    ( Version
    , version
    , major
    , minor
    ) where

import           Core.Include
import           Data.Class
import           Data.Record
import           Ivory.Language



type VersionStruct = "version_struct"


type Version = Version' VersionStruct

newtype Version' v = Version {getVersion :: Record v}


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



instance Include (Version' v) where
    include (Version v) = do
        defStruct (Proxy :: Proxy VersionStruct)
        include v


instance IvoryStruct v => Rec Version' v where
  getRecord = getRecord . getVersion
