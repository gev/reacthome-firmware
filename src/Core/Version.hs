{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Core.Version where

import Control.Monad.State
import Core.Context
import Data.Record
import GHC.TypeNats
import Ivory.Language

type VersionStruct = "version_struct"

type Version = Record VersionStruct

[ivory|
    struct version_struct 
    { major :: Uint8
    ; minor :: Uint8
    }
|]

version_ :: (MonadState Context m) => String -> m Version
version_ name = do
    addStruct (Proxy :: Proxy VersionStruct)
    record_ name

version :: (MonadState Context m) => String -> (Uint8, Uint8) -> m Version
version name (major', minor') = do
    addStruct (Proxy :: Proxy VersionStruct)
    record
        name
        [ major .= ival major'
        , minor .= ival minor'
        ]
