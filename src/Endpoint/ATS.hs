{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Endpoint.ATS where

import           Control.Monad.Writer (MonadWriter)
import           Core.Context
import           Data.Buffer
import           Data.Value
import           Endpoint.DInputs
import           Endpoint.Relays
import           Ivory.Language
import           Ivory.Stdlib



modeNone  = 0x00 :: Uint8
mode_N1_G = 0x11 :: Uint8
mode_N2   = 0x20 :: Uint8
mode_N2_G = 0x21 :: Uint8



data ATS = ATS
    { mode    :: Value Uint8
    , payload :: Buffer 2 Uint8
    }



mkATS :: (MonadWriter Context m) => m ATS
mkATS = do
    mode <- value "ats_mode" modeNone
    payload <- buffer "ats_payload"
    pure ATS {mode, payload}



message :: ATS -> Ivory eff (Buffer 2 Uint8)
message ATS{..} = do
    store (payload ! 0) 0x4
    store (payload ! 1) =<< deref mode
    pure payload



manageATS :: ATS -> DInputs -> Relays -> Ivory eff ()
manageATS ATS{..} inputs relays = do
    mode' <- deref mode
    cond_ [ mode' ==? mode_N1_G ==> pure ()
          , mode' ==? mode_N2   ==> pure ()
          , mode' ==? mode_N2_G ==> pure ()
          ]
