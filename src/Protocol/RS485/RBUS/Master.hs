
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Protocol.RS485.RBUS.Master where

import           Control.Monad.Writer
import           Core.Context
import           Core.Version                        (Version, major, minor,
                                                      version_)
import           Data.Buffer
import           Data.Record
import           Data.Value
import           GHC.TypeNats
import           Interface.Mac
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RS485.RBUS
import           Protocol.RS485.RBUS.Master.MacTable (MacTable, macTable)
import           Util.CRC16



data Master n = Master
    { name          :: String
    , mac           :: Mac
    , model         :: Value       Uint8
    , version       :: Version
    , address       :: Value       Uint8
    , state         :: Value       Uint8
    , phase         :: Value       Uint8
    , index         :: Value       Uint8
    , size          :: Value       Uint8
    , buff          :: Buffer   n  Uint8
    , tidRx         :: Values  255 Sint16
    , tidTx         :: Values  255 Uint8
    , crc           :: Record      CRC16
    , tmp           :: Value       Uint8
    , table         :: MacTable
    , onMessage     :: Buffer   n  Uint8 -> Uint8 -> IBool -> forall s. Ivory (ProcEffects s ()) ()
    , onConfirm     :: forall eff. Ivory eff ()
    , onDiscovery   :: forall eff. Mac -> Value Uint8 -> Version -> Ivory eff ()
    , onPing        :: forall eff. Ivory eff ()
    }


rxPreamble :: Preamble
rxPreamble = preambleSlave

txPreamble :: Preamble
txPreamble = preambleMaster


master :: (MonadWriter Context m, KnownNat n)
       => String
       -> (Buffer n Uint8 -> Uint8 -> IBool -> forall s. Ivory (ProcEffects s ()) ())
       -> (forall eff. Ivory eff ())
       -> (forall eff. Mac -> Value Uint8 -> Version -> Ivory eff ())
       -> (forall eff. Ivory eff ())
       -> m (Master n)
master id onMessage onConfirm onDiscovery onPing = do
    let name = "protocol_" <> id
    mac      <- buffer     (name <> "_mac"      )
    model    <- value_     (name <> "_model"    )
    version  <- version_   (name <> "_version"  )
    address  <- value_     (name <> "_address"  )
    state    <- value      (name <> "_state"    ) readyToReceive
    phase    <- value      (name <> "_phase"    ) waitingAddress
    index    <- value      (name <> "_index"    ) 0
    size     <- value      (name <> "_size"     ) 0
    buff     <- buffer     (name <> "_message"  )
    tidRx    <- values     (name <> "_tid_rx"   ) $ replicate 255 (-1)
    tidTx    <- values     (name <> "_tid_tx"   ) $ replicate 255   0
    crc      <- makeCRC16  (name <> "_crc"      )
    tmp      <- value      (name <> "_tmp"      ) 0
    table    <- macTable   (name <> "_mac_table") 1
    let master = Master { name, mac, model, version, address
                        , state, phase, index, size
                        , buff, tidRx, tidTx, crc, tmp, table
                        , onMessage, onConfirm, onDiscovery, onPing
                        }
    pure master
