{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Endpoint.ATS where

import           Control.Monad.Writer (MonadWriter)
import           Core.Context
import           Data.Buffer
import           Data.Value
import           Ivory.Language



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
    store (payload ! 1) =<< deref mode
    pure payload
