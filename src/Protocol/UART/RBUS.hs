{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Protocol.UART.RBUS where

import           Control.Monad.Writer
import           Core.Context
import           Data.Buffer
import           Data.Record
import           Data.Value
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib
import           Util.CRC16



newtype Preamble = Preamble
    { message   :: Uint8
    }

preamble = Preamble { message = 0xa5 }


readyToReceive      = 0x00 :: Uint8
receivingMessage    = 0x01 :: Uint8

waitingTid          = 0x01 :: Uint8
waitingSize         = 0x02 :: Uint8
waitingData         = 0x03 :: Uint8
waitingMsbCRC       = 0x04 :: Uint8
waitingLsbCRC       = 0x05 :: Uint8



data RBUS n = RBUS
    { name          :: String
    , state         :: Value     Uint8
    , phase         :: Value     Uint8
    , index         :: Value     Uint8
    , size          :: Value     Uint8
    , buff          :: Buffer  n Uint8
    , tidRx         :: Value     Sint16
    , tidTx         :: Value     Uint8
    , crc           :: Record    CRC16
    , tmp           :: Value     Uint8
    , onMessage     :: Buffer n Uint8 -> Uint8 -> IBool -> forall s. Ivory (ProcEffects s ()) ()
    }



rbus :: (MonadWriter Context m, KnownNat n)
      => String
      -> (Buffer n Uint8 -> Uint8 -> IBool -> forall s. Ivory (ProcEffects s ()) ())
      -> m (RBUS n)
rbus id onMessage = do
    let name = "protocol_" <> id
    state    <- value      (name <> "_state"     )   readyToReceive
    phase    <- value      (name <> "_phase"     )   waitingTid
    index    <- value      (name <> "_index"     )   0
    size     <- value      (name <> "_size"      )   0
    buff     <- buffer     (name <> "_message"   )
    tidRx    <- value      (name <> "_tid_rx"    ) (-1)
    tidTx    <- value      (name <> "_tid_tx"    )   0
    crc      <- makeCRC16  (name <> "_crc"       )
    tmp      <- value      (name <> "_tmp"       )   0
    let rbus  = RBUS { name, state, phase, index, size
                     , buff, tidRx, tidTx, crc, tmp
                     , onMessage
                     }
    pure rbus
