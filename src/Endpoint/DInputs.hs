{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Endpoint.DInputs where

import           Control.Monad.Writer
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
    ; synced          :: IBool
    }
|]



data DInputs = DInputs
    { runDInputs :: RunRecords DInputStruct
    , payload    :: Buffer 3 Uint8
    }

dinputs :: MonadWriter Context m => String -> Int -> m DInputs
dinputs name n = do
    addStruct (Proxy :: Proxy DInputStruct)
    let runDInputs = runRecords name $ replicate n go
    payload       <- buffer "dinput_message"
    let dinputs    = DInputs {runDInputs, payload}
    runDInputs addArea
    pure dinputs
    where go = [ state  .= ival false
               , synced .= ival true
               ]



message :: DInputs -> Uint8 -> Ivory eff (Buffer 3 Uint8)
message DInputs{..} i = do
    runDInputs $ \r -> do
        let dinput = addrOf r ! toIx i
        pack   payload 1 (1 :: Uint8)
        pack   payload 1 $ i + 1
        pack   payload 2 =<< deref (dinput ~> state)
    pure payload