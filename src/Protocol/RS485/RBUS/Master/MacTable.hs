{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Protocol.RS485.RBUS.Master.MacTable where

import           Control.Monad.RWS
import           Core.Context
import           Core.Version
import           Data.Matrix
import           Data.Value
import           Interface.MCU
import           Ivory.Language


data MacTable = MacTable
    { size     :: Value          Uint32
    , tAddress :: Values   255   Uint8
    , tMac     :: Matrix   255 6 Uint8
    , tModel   :: Values   255   Uint8
    , tVersion :: Versions 255
    }


macTable :: MonadWriter Context m => String -> m MacTable
macTable id = do
    let name = id <> "_table"
    size     <- value     (name <> "_size"   ) 0
    tAddress <- values_   (name <> "_address")
    tMac     <- matrix_   (name <> "_mac"    )
    tModel   <- values_   (name <> "_model"  )
    tVersion <- versions_ (name <> "_version")
    pure MacTable {size, tAddress, tMac, tModel, tVersion}
