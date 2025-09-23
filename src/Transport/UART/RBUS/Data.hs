{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Transport.UART.RBUS.Data where

import Control.Monad.State (MonadState)
import Core.Context
import Core.Version (Version)
import Data.Buffer
import Data.Queue
import Data.Value
import GHC.TypeNats
import Interface.MCU
import Interface.Mac
import Interface.SystemClock
import Interface.UART
import Ivory.Language
import qualified Protocol.UART.RBUS as U

data RBUS q l where
    RBUS ::
        (UART (u rn tn), KnownNat rn, KnownNat tn) =>
        { name :: String
        , speed :: Uint32
        , model :: Value Uint8
        , version :: Version
        , mac :: Mac
        , clock :: SystemClock
        , uart :: u rn tn
        , protocol :: U.RBUS 255
        , msgQueue :: Queue q (Messages q)
        , msgBuff :: Buffer l Uint8
        , msgIndex :: Value Uint16
        , discoveryBuff :: Buffer 10 Uint8
        , txLock :: Value IBool
        , rxTimestamp :: Value Uint32
        } ->
        RBUS q l

data Messages n = Messages
    { msgOffset :: Buffer n Uint16
    , msgSize :: Buffer n Uint8
    }

messages ::
    (KnownNat n, MonadState Context m) =>
    String ->
    m (Messages n)
messages name = do
    msgOffset <- buffer (name <> "_msg_offset")
    msgSize <- buffer (name <> "_msg_size")
    pure
        Messages
            { msgOffset
            , msgSize
            }
