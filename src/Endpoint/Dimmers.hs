{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Endpoint.Dimmers where

import           Control.Monad.Writer
import           Core.Context
import           Data.Buffer
import           Data.Record
import           Data.Serialize
import           Ivory.Language


type DimmerStruct = "dimmer_struct"

[ivory|
    struct dimmer_struct
    { mode     :: Uint8
    ; value    :: Uint8
    ; velocity :: Uint8
    ; group    :: Uint8
    ; synced   :: IBool
    }
|]



data Dimmers = Dimmers
    { runDimmers :: RunRecords DimmerStruct
    , payload    :: Buffer 6 Uint8
    }

dimmers :: MonadWriter Context m => String -> Int -> m Dimmers
dimmers name n = do
    addStruct (Proxy :: Proxy DimmerStruct)
    let runDimmers = runRecords name $ replicate n go
    payload       <- buffer "dimmer_message"
    let dimmers    = Dimmers {runDimmers, payload}
    runDimmers addArea
    pure dimmers
    where go = [ mode     .= ival 0
               , value    .= ival 0
               , velocity .= ival 0
               , group    .= ival 1
               , synced   .= ival true
               ]



message :: Dimmers -> Uint8 -> Ivory eff (Buffer 6 Uint8)
message Dimmers{..} i = do
    runDimmers $ \d -> do
        let dimmer = addrOf d ! toIx i
        pack payload 0 (0xd0 :: Uint8)
        pack payload 1 $ i + 1
        pack payload 2 =<< deref (dimmer ~> group)
        pack payload 3 =<< deref (dimmer ~> mode)
        pack payload 4 =<< deref (dimmer ~> value)
        pack payload 5 =<< deref (dimmer ~> velocity)
    pure payload
