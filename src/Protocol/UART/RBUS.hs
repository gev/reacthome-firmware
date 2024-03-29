{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Protocol.UART.RBUS where

import           Control.Monad.State (MonadState)
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

waitingSize         = 0x01 :: Uint8
waitingData         = 0x02 :: Uint8
waitingMsbCRC       = 0x03 :: Uint8
waitingLsbCRC       = 0x04 :: Uint8



data RBUS n = RBUS
    { name      :: String
    , state     :: Value    Uint8
    , phase     :: Value    Uint8
    , offset    :: Value    Uint8
    , size      :: Value    Uint8
    , buff      :: Buffer n Uint8
    , crc       :: Record   CRC16
    , valid     :: Value    IBool
    , onMessage :: Buffer n Uint8 -> Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }



rbus :: (MonadState Context m, KnownNat n)
      => String
      -> (Buffer n Uint8 -> Uint8 -> forall s. Ivory (ProcEffects s ()) ())
      -> m (RBUS n)
rbus id onMessage = do
    let name = id <> "_protocol"
    state    <- value     (name <> "_state"     ) readyToReceive
    phase    <- value     (name <> "_phase"     ) waitingSize
    offset   <- value     (name <> "_offset"    ) 0
    size     <- value     (name <> "_size"      ) 0
    buff     <- buffer    (name <> "_message"   )
    crc      <- makeCRC16 (name <> "_crc"       )
    valid    <- value     (name <> "_valid"     ) true
    let rbus  = RBUS { name, state, phase, offset, size
                     , buff, crc, valid
                     , onMessage
                     }
    pure rbus
