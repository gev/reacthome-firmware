{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

module Endpoint.Groups where

import           Control.Monad.Writer
import           Core.Context
import           Data.Buffer
import           Data.Record
import           Data.Serialize
import           GHC.TypeNats
import           Ivory.Language



type GroupStruct = "group_struct"

[ivory|
    struct group_struct
    { enabled   :: IBool
    ; delay     :: Uint32
    ; synced    :: IBool
    }
|]



data Groups = Groups
    { runGroups :: RunRecords GroupStruct
    , payload   :: Buffer 7 Uint8
    }

groups :: MonadWriter Context m =>  String -> Int -> m Groups
groups name n = do
    addStruct (Proxy :: Proxy GroupStruct)
    let runGroups = runRecords name $ replicate n go
    payload      <- buffer "group_message"
    runGroups addArea
    pure Groups { runGroups, payload }
    where go =  [ enabled   .= ival false
                , delay     .= ival 0
                , synced    .= ival true
                ]



message :: Groups -> Uint8 -> Ivory eff (Buffer 7 Uint8)
message (Groups runGroup payload) i = do
    runGroup $ \r -> do
        let group = addrOf r ! toIx i
        pack   payload 0 (2 :: Uint8)
        pack   payload 1 $ i + 1
        pack   payload 2 =<< deref (group ~> enabled)
        packLE payload 3 =<< deref (group ~> delay)
    pure payload
