{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Transport.RS485.RBUS.Data where

import Control.Monad.State (MonadState)
import Core.Context
import Data.Buffer
import Data.Queue
import Data.Value
import GHC.TypeLits (KnownNat)
import Interface.RS485
import Interface.SystemClock
import Ivory.Language
import Protocol.RS485.RBUS.Slave

data RBUS = RBUS
    { clock :: SystemClock
    , rs :: RS485 256 300
    , protocol :: Slave 255
    , msgQueue :: Queue 32 (Messages 32)
    , msgBuff :: Buffer 300 Uint8
    , msgIndex :: Value Uint16
    , initBuff :: Buffer 1 Uint8
    , rxLock :: Value IBool
    , txLock :: Value IBool
    , rxTimestamp :: Value Uint32
    , txTimestamp :: Value Uint32
    , initTimestamp :: Value Uint32
    , shouldConfirm :: Value IBool
    , msgConfirmed :: Value IBool
    , waitingConfirm :: Value IBool
    , shouldInit :: Value IBool
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
