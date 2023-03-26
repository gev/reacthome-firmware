{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Protocol.RS485.RBUS.Master.Tx where

import           Data.Buffer
import           GHC.TypeNats
import           Interface.Mac
import           Ivory.Language
import           Protocol.RS485.RBUS
import           Protocol.RS485.RBUS.Master
import           Util.CRC16


transmitMessage :: KnownNat l
                => Uint8
                -> Buffer l Uint8
                -> Master n
                -> (Uint8 -> forall eff. Ivory eff ())
                -> Ivory (ProcEffects s ()) ()
transmitMessage address' payload (Master {..}) transmit = do
    crc <- local $ istruct initCRC16
    let transmit' :: Uint8 -> Ivory eff ()
        transmit' v = updateCRC16 crc v >> transmit v
    transmit' $ message txPreamble
    transmit' address'
    let tidTx' = tidTx ! toIx address'
    id <- deref tidTx'
    transmit' id
    store tidTx' $ id + 1
    transmit' $ arrayLen payload
    arrayMap $ \ix -> transmit' =<< deref (payload ! ix)
    transmit =<< deref (crc ~> msb)
    transmit =<< deref (crc ~> lsb)



transmitDiscovery :: Mac
                  -> Uint8
                  -> Master n
                  -> (Uint8 -> forall eff. Ivory eff ())
                  -> Ivory (ProcEffects s ()) ()
transmitDiscovery mac' address' (Master {..}) transmit = do
    crc <- local $ istruct initCRC16
    let transmit' :: Uint8 -> Ivory eff ()
        transmit' v = updateCRC16 crc v >> transmit v
    transmit' $ discovery txPreamble
    arrayMap $ \ix -> transmit' =<< deref (mac' ! ix)
    transmit' address'
    transmit =<< deref (crc ~> msb)
    transmit =<< deref (crc ~> lsb)



transmitPing :: Uint8
             -> Master n
             -> (Uint8 -> forall eff. Ivory eff ())
             -> Ivory (ProcEffects s ()) ()
transmitPing address' (Master {..}) transmit = do
    crc <- local $ istruct initCRC16
    let transmit' :: Uint8 -> Ivory eff ()
        transmit' v = updateCRC16 crc v >> transmit v
    transmit' $ ping txPreamble
    transmit' address'
    transmit =<< deref (crc ~> msb)
    transmit =<< deref (crc ~> lsb)


transmitConfirm :: Uint8
                -> Master n
                -> (Uint8 -> forall eff. Ivory eff ())
                -> Ivory (ProcEffects s ()) ()
transmitConfirm address' (Master {..}) transmit = do
    crc <- local $ istruct initCRC16
    let transmit' :: Uint8 -> Ivory eff ()
        transmit' v = updateCRC16 crc v >> transmit v
    transmit' $ confirm txPreamble
    transmit' address'
    transmit =<< deref (crc ~> msb)
    transmit =<< deref (crc ~> lsb)
