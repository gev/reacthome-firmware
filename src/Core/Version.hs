{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}

module Core.Version
    ( Version
    , version
    , major
    , minor
    ) where

import           Control.Monad.Writer
import           Core.Context
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


version :: MonadWriter Context m => String -> Uint8 -> Uint8 -> m Version
version name maj min = do
    let v = record name [ major .= ival maj
                        , minor .= ival min
                        ]
    addStruct (Proxy :: Proxy VersionStruct)
    addArea v
    pure v
