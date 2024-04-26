{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Endpoint.DInputs where

import           Control.Monad.State (MonadState)
import           Core.Actions
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



data DInputs n = DInputs
    { dInputs :: Records n DInputStruct
    , payload :: Buffer 3 Uint8
    }



dinputs :: (MonadState Context m, KnownNat n) => String -> Int -> m ( DInputs n )
dinputs name n = do
    addStruct (Proxy :: Proxy DInputStruct)
    dInputs       <-  records name $ replicate n go
    payload       <- buffer "dinput_message"
    let dinputs    = DInputs {dInputs, payload}
    pure dinputs
    where go = [ state     .= ival false
               , timestamp .= ival 0
               , synced    .= ival false
               ]



message :: KnownNat n => DInputs n -> Uint8 -> Ivory eff (Buffer 3 Uint8)
message DInputs{..} i = do
    let dinput = dInputs ! toIx i
    pack   payload 0 actionDi
    pack   payload 1 $ i + 1
    pack   payload 2 =<< deref (dinput ~> state)
    pure payload
