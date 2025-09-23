{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Endpoint.Groups where

import Control.Monad.State
import Core.Actions
import Core.Context
import Data.Buffer
import Data.Record
import Data.Serialize
import GHC.TypeNats
import Ivory.Language

type GroupStruct = "group_struct"

[ivory|
    struct group_struct
    { enabled :: IBool
    ; delay :: Uint32
    ; synced :: IBool
    }
|]

data Groups n = Groups
    { groups :: Records n GroupStruct
    , payload :: Buffer 7 Uint8
    }

mkGroups :: (MonadState Context m, KnownNat n) => String -> m (Groups n)
mkGroups name = do
    addStruct (Proxy :: Proxy GroupStruct)
    groups <-
        records'
            name
            [ enabled .= ival false
            , delay .= ival 0
            , synced .= ival false
            ]
    payload <- buffer "group_message"
    pure Groups{groups, payload}

message :: (KnownNat n) => Groups n -> Uint8 -> Ivory eff (Buffer 7 Uint8)
message Groups{..} i = do
    let group = groups ! toIx i
    pack payload 0 actionGroup
    pack payload 1 $ i + 1
    pack payload 2 =<< deref (group ~> enabled)
    packLE payload 3 =<< deref (group ~> delay)
    pure payload
