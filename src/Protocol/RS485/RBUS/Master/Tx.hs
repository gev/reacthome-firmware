{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Protocol.RS485.RBUS.Master.Tx where

import           Data.Buffer
import           GHC.TypeNats
import           Interface.Mac
import           Ivory.Language
import           Protocol.RS485.RBUS
import           Protocol.RS485.RBUS.Master
import           Protocol.RS485.RBUS.Master.MacTable as T
import           Util.CRC16



transmitMessage :: KnownNat l
                => Uint8
                -> Buffer l Uint8
                -> Master n
                -> (Uint8 -> forall eff. Ivory eff ())
                -> Ivory (ProcEffects s ()) ()
transmitMessage address' payload Master{..} =
    run $ \transmit -> do
        transmit $ message txPreamble
        transmit address'
        let tidTx' = tidTx ! toIx address'
        id <- deref tidTx'
        transmit id
        store tidTx' $ id + 1
        transmit $ arrayLen payload
        arrayMap $ \ix -> transmit =<< deref (payload ! ix)



transmitDiscovery :: Uint8
                  -> Master n
                  -> (Uint8 -> forall eff. Ivory eff ())
                  -> Ivory (ProcEffects s ()) ()
transmitDiscovery address' Master{..} =
    run $ \transmit -> lookupMac table address' $ \rec -> do
        transmit $ discovery txPreamble
        arrayMap $ \ix -> transmit =<< deref (rec ~> T.mac ! ix)
        transmit address'



transmitPing :: Uint8
             -> Master n
             -> (Uint8 -> forall eff. Ivory eff ())
             -> Ivory (ProcEffects s ()) ()
transmitPing address' m =
    run $ \transmit -> do
        transmit $ ping txPreamble
        transmit address'



transmitConfirm :: Uint8
                -> Master n
                -> (Uint8 -> forall eff. Ivory eff ())
                -> Ivory (ProcEffects s ()) ()
transmitConfirm address' m =
    run $ \transmit -> do
        transmit $ confirm txPreamble
        transmit address'



run :: ((Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s ()) ())
    -> (Uint8 -> forall eff. Ivory eff ())
    -> Ivory (ProcEffects s ()) ()
run tx transmit = do
    crc <- local $ istruct initCRC16
    tx $ \v -> updateCRC16 crc v >> transmit v
    transmit =<< deref (crc ~> msb)
    transmit =<< deref (crc ~> lsb)
