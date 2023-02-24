{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}


module Protocol.RBUS.Slave.Tx where
import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language
import           Protocol.RBUS
import           Protocol.RBUS.Slave
import           Util.CRC16


transmitMessage :: KnownNat l
                => Buffer l Uint8
                -> Slave n
                -> (Uint8 -> forall eff. Ivory eff ())
                -> Ivory (ProcEffects s ()) ()
transmitMessage payload (Slave{address, tidTx}) transmit = do
    let payload' = addrOf payload
    crc <- local $ istruct initCRC16
    let transmit' :: Uint8 -> Ivory eff ()
        transmit' v = updateCRC16 crc v >> transmit v
    transmit' $ message txPreamble
    transmit' =<< deref (addrOf address)
    id <- deref (addrOf tidTx)
    transmit' id
    store (addrOf tidTx) $ id + 1
    transmit' $ arrayLen payload'
    arrayMap $ \ix -> transmit' =<< deref (payload' ! ix)
    transmit =<< deref (crc ~> msb)
    transmit =<< deref (crc ~> lsb)



transmitDiscovery :: Slave n -> (Uint8 -> Ivory (AllowBreak eff) ()) -> Ivory eff ()
transmitDiscovery = transmit' . buffDisc

transmitPing :: Slave n -> (Uint8 -> Ivory (AllowBreak eff) ()) -> Ivory eff ()
transmitPing = transmit' . buffPing

transmitConfirm :: Slave n -> (Uint8 -> Ivory (AllowBreak eff) ()) -> Ivory eff ()
transmitConfirm = transmit' . buffConf

transmit' :: KnownNat n
          => Buffer n Uint8
          -> (Uint8 -> Ivory (AllowBreak eff) ())
          -> Ivory eff ()
transmit' buff transmit =
    arrayMap $ \ix -> transmit =<< deref (addrOf buff ! ix)
