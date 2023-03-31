{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}

module Core.Version
    ( Version
    , Versions
    , version_
    , version
    , versions_
    , major
    , minor
    ) where

import           Control.Monad.Writer
import           Core.Context
import           Data.Record
import           GHC.TypeNats
import           Ivory.Language



type VersionStruct = "version_struct"


type Version    = Record   VersionStruct
type Versions n = Records n VersionStruct


[ivory|
    struct version_struct {
        major :: Uint8;
        minor :: Uint8;
    }
|]


versions_ :: (MonadWriter Context m, KnownNat n) => String -> m (Versions n)
versions_ name = do
    addStruct (Proxy :: Proxy VersionStruct)
    records_ name

version_ :: MonadWriter Context m => String -> m Version
version_ name = do
    addStruct (Proxy :: Proxy VersionStruct)
    record_ name


version :: MonadWriter Context m => String -> (Uint8, Uint8) -> m Version
version name (major', minor') = do
    addStruct (Proxy :: Proxy VersionStruct)
    record name [ major .= ival major'
                , minor .= ival minor'
                ]
