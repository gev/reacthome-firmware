{-# LANGUAGE DataKinds #-}

module Domain where

import           Interface.SystemClock
import           Ivory.Language
import           Util.Data.Buffer
import           Util.Data.Record
import           Util.Version



data Domain mcu = Domain
    { model   :: Uint8
    , version :: Record Version
    , mcu     :: mcu
    , clock   :: SystemClock
    , mac     :: Buffer 6 Uint8
    }
