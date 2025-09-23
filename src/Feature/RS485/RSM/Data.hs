{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Feature.RS485.RSM.Data where

import Core.Transport
import Data.Buffer
import Data.Queue
import Data.Value
import Interface.RS485
import Interface.SystemClock
import Ivory.Language

data RSM = forall t. (LazyTransport t, Transport t) => RSM
     { index :: Int
     , clock :: SystemClock
     , rs :: RS485 32 300
     , baudrate :: Value Uint32
     , lineControl :: Value Uint8
     , rsBuff :: Buffer 253 Uint8
     , rsSize :: Value Uint8
     , rxLock :: Value IBool
     , txLock :: Value IBool
     , rxTimestamp :: Value Uint32
     , txTimestamp :: Value Uint32
     , shouldInit :: Value IBool
     , synced :: Value IBool
     , payload :: Buffer 8 Uint8
     , transport :: t
     }
