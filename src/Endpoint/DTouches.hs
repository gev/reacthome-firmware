{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Endpoint.DTouches where

import           Control.Monad.State (MonadState)
import           Core.Actions
import           Core.Context
import           Data.Buffer
import           Data.Record
import           Data.Serialize
import           GHC.TypeNats
import           Ivory.Language



type DTouchStruct = "dtouch_struct"

[ivory|
    struct dtouch_struct
    { state           :: IBool
    ; timestamp       :: Uint16
    ; timeMin         :: IFloat
    ; timeMax         :: IFloat
    ; time            :: IFloat
    ; synced          :: IBool
    }
|]



data DTouches n = DTouches
    { dtouches :: Records n DTouchStruct
    , payload :: Buffer 3 Uint8
    }



mkDTouches :: (MonadState Context m, KnownNat n) => String -> m ( DTouches n )
mkDTouches name = do
    addStruct (Proxy :: Proxy DTouchStruct)
    dtouches <-  records' name [ state    .= ival false
                              , timestamp .= ival 0
                              , timeMin   .= ival 0xFFFF
                              , timeMax   .= ival 0
                              , time      .= ival 0
                              , synced    .= ival false
                              ]
    payload <- buffer "dtouch_message"
    pure DTouches {dtouches, payload}



message :: KnownNat n => DTouches n -> Uint8 -> Ivory eff (Buffer 3 Uint8)
message DTouches{..} i = do
    let dtouch = dtouches ! toIx i
    pack   payload 0 actionDi
    pack   payload 1 $ i + 1
    pack   payload 2 =<< deref (dtouch ~> state)
    pure payload


-- average :: IFloat -> IFloat -> IFloat
-- average a b = do
--     let alpha = 0.1
--     a * (1 - alpha) + b * alpha