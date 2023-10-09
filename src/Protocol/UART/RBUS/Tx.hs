{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Protocol.UART.RBUS.Tx where

import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language
import           Protocol.UART.RBUS
import           Util.CRC16


transmitMessage :: KnownNat l
                => Buffer l Uint8
                -> Uint8
                -> RBUS n
                -> (Uint8 -> forall eff. Ivory eff ())
                -> Ivory (ProcEffects s ()) ()
transmitMessage payload size' RBUS{..} transmit = do
    crc <- local $ istruct initCRC16
    let transmit' v = updateCRC16 crc v >> transmit v
    transmit' $ message preamble
    transmit' size'
    for (toIx size') $ \ix -> transmit' =<< deref (payload ! ix)
    transmit =<< deref (crc ~> msb)
    transmit =<< deref (crc ~> lsb)


transmitMessage' :: ((Uint8 -> forall eff. Ivory eff ()) -> forall eff. Ivory eff ())
                 -> RBUS n
                 -> (Uint8 -> forall eff. Ivory eff ())
                 -> Ivory (ProcEffects s ()) ()
transmitMessage' run RBUS{..} transmit = do
    crc <- local $ istruct initCRC16
    let transmit' v = updateCRC16 crc v >> transmit v
    transmit' $ message preamble
    run transmit'
    transmit =<< deref (crc ~> msb)
    transmit =<< deref (crc ~> lsb)