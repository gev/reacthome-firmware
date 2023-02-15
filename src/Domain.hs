{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}

module Domain where

import           Data.Buffer
import           Data.Record
import           Data.Value
import           Include
import           Initialize
import qualified Interface.Mac         as M
import qualified Interface.MCU         as I
import           Interface.SystemClock
import           Ivory.Language
import qualified Util.Version          as V


data Domain mcu = Domain
    { model   :: Value  Uint8
    , version :: V.Version
    , mac     :: M.Mac
    , mcu     :: mcu
    }


domain :: I.MCU mcu => Uint8 -> (Uint8, Uint8) -> mcu -> Domain mcu
domain model (major, minor) mcu = Domain
    { model   = value "model" model
    , version = V.version "version" major minor
    , mac     = I.mac mcu "mac"
    , mcu     = mcu
    }



instance Include (Domain mcu) where
    include (Domain {model, version, mac}) = do
        include model
        include version
        include mac


instance Initialize (Domain mcu) where
    initialize = initialize . mac
