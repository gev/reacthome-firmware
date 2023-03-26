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
transmitMessage address' payload (Master {..}) =
    transmit' $ \tx -> do
        tx $ message txPreamble
        tx address'
        let tidTx' = tidTx ! toIx address'
        id <- deref tidTx'
        tx id
        store tidTx' $ id + 1
        tx $ arrayLen payload
        arrayMap $ \ix -> tx =<< deref (payload ! ix)



transmitDiscovery :: Mac
                  -> Uint8
                  -> Master n
                  -> (Uint8 -> forall eff. Ivory eff ())
                  -> Ivory (ProcEffects s ()) ()
transmitDiscovery mac' address' (Master {..}) =
    transmit' $ \tx -> do
        tx $ discovery txPreamble
        arrayMap $ \ix -> tx =<< deref (mac' ! ix)
        tx address'



transmitPing :: Uint8
             -> Master n
             -> (Uint8 -> forall eff. Ivory eff ())
             -> Ivory (ProcEffects s ()) ()
transmitPing address' (Master {..}) =
    transmit' $ \tx -> do
        tx $ ping txPreamble
        tx address'



transmitConfirm :: Uint8
                -> Master n
                -> (Uint8 -> forall eff. Ivory eff ())
                -> Ivory (ProcEffects s ()) ()
transmitConfirm address' (Master {..}) =
    transmit' $ \tx -> do
        tx $ confirm txPreamble
        tx address'



transmit' :: ((Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s ()) ())
          -> (Uint8 -> forall eff. Ivory eff ())
          -> Ivory (ProcEffects s ()) ()
transmit' tx transmit = do
    crc <- local $ istruct initCRC16
    tx $ \v -> updateCRC16 crc v >> transmit v
    transmit =<< deref (crc ~> msb)
    transmit =<< deref (crc ~> lsb)
