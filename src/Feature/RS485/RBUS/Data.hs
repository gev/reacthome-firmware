module Feature.RS485.RBUS.Data where

import Control.Monad.State (MonadState)
import Core.Context
import Core.Transport
import Data.Buffer
import Data.Queue
import Data.Value
import GHC.TypeNats
import Interface.RS485
import Interface.SystemClock
import Ivory.Language
import Protocol.RS485.RBUS.Master

modeNone = 2 :: Uint8
modeRBUS = 1 :: Uint8
modeRS485 = 0 :: Uint8

data RBUS = forall t. (LazyTransport t, Transport t) => RBUS
    { index :: Int
    , clock :: SystemClock
    , rs :: RS485 300 300
    , mode :: Value Uint8
    , baudrate :: Value Uint32
    , lineControl :: Value Uint8
    , protocol :: Master 255
    , msgQueue :: Queue 64 (Messages 64)
    , msgWaitingConfirm :: Values 255 IBool
    , msgConfirmed :: Values 255 IBool
    , msgBuff :: Buffer 3072 Uint8
    , msgIndex :: Value Uint16
    , rsBuff :: Buffer 253 Uint8
    , rsSize :: Value Uint8
    , rxLock :: Value IBool
    , txLock :: Value IBool
    , rxTimestamp :: Value Uint32
    , txTimestamp :: Value Uint32
    , shouldDiscovery :: Value IBool
    , shouldConfirm :: Value IBool
    , shouldPing :: Value Uint8
    , discoveryAddress :: Value Uint8
    , confirmAddress :: Value Uint8
    , pingAddress :: Value Uint8
    , shouldInit :: Value IBool
    , synced :: Value IBool
    , payload :: Buffer 8 Uint8
    , transport :: t
    }

data Messages n = Messages
    { msgOffset :: Buffer n Uint16
    , msgSize :: Buffer n Uint8
    , msgTTL :: Buffer n Uint8
    }

messages ::
    (KnownNat n, MonadState Context m) =>
    String ->
    m (Messages n)
messages name = do
    msgOffset <- buffer (name <> "_msg_offset")
    msgSize <- buffer (name <> "_msg_size")
    msgTTL <- buffer (name <> "_msg_ttl")
    pure
        Messages
            { msgOffset
            , msgSize
            , msgTTL
            }
