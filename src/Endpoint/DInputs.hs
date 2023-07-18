{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Endpoint.DInputs where

import           Control.Monad.State (MonadState)
import           Core.Context
import           Data.Buffer
import           Data.Record
import           Data.Serialize
import           GHC.TypeNats
import           Ivory.Language



type DInputStruct = "dinput_struct"

[ivory|
    struct dinput_struct
    { state           :: IBool
    ; timestamp       :: Uint32
    ; synced          :: IBool
    }
|]



data DInputs = DInputs
    { runDInputs :: RunRecords DInputStruct
    , payload    :: Buffer 3 Uint8
    }

dinputs :: MonadState Context m => String -> Int -> m DInputs
dinputs name n = do
    addStruct (Proxy :: Proxy DInputStruct)
    let runDInputs = runRecords name $ replicate n go
    payload       <- buffer "dinput_message"
    let dinputs    = DInputs {runDInputs, payload}
    runDInputs addArea
    pure dinputs
    where go = [ state     .= ival false
               , timestamp .= ival 0
               , synced    .= ival false
               ]



message :: DInputs -> Uint8 -> Ivory eff (Buffer 3 Uint8)
message DInputs{..} i = do
    runDInputs $ \di -> do
        let dinput = addrOf di ! toIx i
        pack   payload 0 (1 :: Uint8)
        pack   payload 1 $ i + 1
        pack   payload 2 =<< deref (dinput ~> state)
    pure payload
