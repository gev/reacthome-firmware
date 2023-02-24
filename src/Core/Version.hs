{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}

module Core.Version
    ( Version
    , version
    , major
    , minor
    ) where

import           Core.Include
import           Data.Record
import           Ivory.Language



type VersionStruct = "version_struct"


type Version = Record VersionStruct


[ivory|
    struct version_struct {
        major :: Uint8;
        minor :: Uint8;
    }
|]


version :: String -> Uint8 -> Uint8 -> Version
version name maj min = record name [ major .= ival maj
                                   , minor .= ival min
                                   ]


instance Include Version where
    include v = do
        defStruct (Proxy :: Proxy VersionStruct)
        defMemArea v
