{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Protocol.RS485.RBUS.Slave.Tx where

import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language
import           Protocol.RS485.RBUS
import           Protocol.RS485.RBUS.Slave
import           Util.CRC16



transmitMessage :: KnownNat l
                => Buffer l Uint8
                -> Uint8
                -> Slave n
                -> (Uint8 -> forall eff. Ivory eff ())
                -> Ivory (ProcEffects s ()) ()
transmitMessage payload size' Slave{..} transmit = do
    crc <- local $ istruct initCRC16
    let transmit' v = updateCRC16 crc v >> transmit v
    transmit' $ message txPreamble
    transmit' =<< deref address
    id <- deref tidTx
    transmit' id
    store tidTx $ id + 1
    transmit' size'
    for (toIx size') $ \ix -> transmit' =<< deref (payload ! ix)
    transmit =<< deref (crc ~> msb)
    transmit =<< deref (crc ~> lsb)


transmitMessage' :: (forall eff. (Uint8 -> forall eff. Ivory eff ()) -> Ivory eff ())
                 -> Slave n
                 -> (Uint8 -> forall eff. Ivory eff ())
                 -> Ivory (ProcEffects s ()) ()
transmitMessage' run Slave{..} transmit = do
    crc <- local $ istruct initCRC16
    let transmit' :: Uint8 -> Ivory eff ()
        transmit' v = updateCRC16 crc v >> transmit v
    transmit' $ message txPreamble
    id <- deref tidTx
    transmit' id
    store tidTx $ id + 1
    run transmit
    transmit =<< deref (crc ~> msb)
    transmit =<< deref (crc ~> lsb)




transmitDiscovery :: Slave n -> (Uint8 -> Ivory (AllowBreak eff) ()) -> Ivory eff ()
transmitDiscovery = transmit . buffDisc

transmitPing :: Slave n -> (Uint8 -> Ivory (AllowBreak eff) ()) -> Ivory eff ()
transmitPing = transmit . buffPing

transmitConfirm :: Slave n -> (Uint8 -> Ivory (AllowBreak eff) ()) -> Ivory eff ()
transmitConfirm = transmit . buffConf

transmit :: KnownNat n
    => Buffer n Uint8
    -> (Uint8 -> Ivory (AllowBreak eff) ())
    -> Ivory eff ()
transmit buff transmit =
    arrayMap $ \ix -> transmit =<< deref (buff ! ix)
