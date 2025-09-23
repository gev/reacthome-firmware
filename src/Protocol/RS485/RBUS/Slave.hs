{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use for_" #-}

module Protocol.RS485.RBUS.Slave where

import Control.Monad.State
import Core.Context
import Core.Version (Version, major, minor)
import Data.Buffer
import Data.Record
import Data.Value
import GHC.TypeNats
import Interface.Mac
import Ivory.Language
import Ivory.Stdlib
import Protocol.RS485.RBUS
import Util.CRC16

data Slave n = Slave
        { name :: String
        , mac :: Mac
        , model :: Value Uint8
        , version :: Version
        , address :: Value Uint8
        , state :: Value Uint8
        , phase :: Value Uint8
        , offset :: Value Uint8
        , size :: Value Uint8
        , buff :: Buffer n Uint8
        , buffConf :: Buffer 4 Uint8
        , buffPing :: Buffer 4 Uint8
        , buffDisc :: Buffer 12 Uint8
        , tidRx :: Value Sint16
        , tidTx :: Value Uint8
        , crc :: Record CRC16
        , valid :: Value IBool
        , tmp :: Value Uint8
        , onMessage :: Buffer n Uint8 -> Uint8 -> IBool -> forall s. Ivory (ProcEffects s ()) ()
        , onConfirm :: forall eff. Ivory eff ()
        , onDiscovery :: forall eff. Ivory eff ()
        , onReceive :: forall eff. Ivory eff ()
        , initConf :: Def ('[] :-> ())
        , initPing :: Def ('[] :-> ())
        }

rxPreamble :: Preamble
rxPreamble = preambleMaster

txPreamble :: Preamble
txPreamble = preambleSlave

slave ::
        (MonadState Context m, KnownNat n) =>
        String ->
        Buffer 6 Uint8 ->
        Value Uint8 ->
        Version ->
        (Buffer n Uint8 -> Uint8 -> IBool -> forall s. Ivory (ProcEffects s ()) ()) ->
        (forall eff. Ivory eff ()) ->
        (forall eff. Ivory eff ()) ->
        (forall eff. Ivory eff ()) ->
        m (Slave n)
slave id mac model version onMessage onConfirm onDiscovery onReceive = do
        let name = id <> "_protocol_slave"
        address <- value (name <> "_address") broadcastAddress
        state <- value (name <> "_state") readyToReceive
        phase <- value (name <> "_phase") waitingAddress
        offset <- value (name <> "_offset") 0
        size <- value (name <> "_size") 0
        buff <- buffer (name <> "_message")
        buffConf <- buffer (name <> "_confirm_tx")
        buffPing <- buffer (name <> "_ping_tx")
        buffDisc <- buffer (name <> "_disc_tx")
        tidRx <- value (name <> "_tid_rx") (-1)
        tidTx <- value (name <> "_tid_tx") 0
        crc <- makeCRC16 (name <> "_crc")
        valid <- value (name <> "_valid") true
        tmp <- value (name <> "_tmp") 0

        addInit (name <> "_disc_tx") $ do
                store (buffDisc ! 0) $ discovery txPreamble
                arrayCopy buffDisc mac 1 $ arrayLen mac
                store (buffDisc ! 7) =<< deref model
                store (buffDisc ! 8) =<< deref (version ~> major)
                store (buffDisc ! 9) =<< deref (version ~> minor)
                calcCRC16 buffDisc

        initConf <- addInit (name <> "_conf_tx") $ do
                store (buffConf ! 0) $ confirm txPreamble
                store (buffConf ! 1) =<< deref address
                calcCRC16 buffConf

        initPing <- addInit (name <> "_ping_tx") $ do
                store (buffPing ! 0) $ ping txPreamble
                store (buffPing ! 1) =<< deref address
                calcCRC16 buffPing

        pure
                Slave
                        { name
                        , mac
                        , model
                        , version
                        , address
                        , state
                        , phase
                        , offset
                        , size
                        , buff
                        , buffConf
                        , buffPing
                        , buffDisc
                        , tidRx
                        , tidTx
                        , crc
                        , valid
                        , tmp
                        , onMessage
                        , onConfirm
                        , onDiscovery
                        , onReceive
                        , initConf
                        , initPing
                        }

hasAddress :: Slave n -> Ivory eff IBool
hasAddress Slave{..} = do
        a <- deref address
        pure $ a /=? broadcastAddress
