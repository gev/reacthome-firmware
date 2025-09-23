module Protocol.RS485.RBUS.Master.Tx where

import Data.Buffer
import GHC.TypeNats
import Interface.Mac
import Ivory.Language
import Ivory.Language.Array (IxRep)
import Protocol.RS485.RBUS
import Protocol.RS485.RBUS.Master
import Protocol.RS485.RBUS.Master.MacTable as T
import Util.CRC16

transmitMessage ::
    (KnownNat l) =>
    Uint8 ->
    Buffer l Uint8 ->
    Ix l ->
    Uint8 ->
    Master n ->
    (Uint8 -> forall eff. Ivory eff ()) ->
    Ivory (ProcEffects s t) ()
transmitMessage address' payload' offset' size' Master{..} =
    run $ \transmit -> do
        transmit $ message txPreamble
        transmit address'
        let tidTx' = tidTx ! toIx address'
        id <- deref tidTx'
        transmit id
        store tidTx' $ id + 1
        transmit size'
        for (toIx size') $ \ix ->
            transmit
                =<< deref (payload' ! (offset' + ix))

transmitDiscovery ::
    Uint8 ->
    Master n ->
    (Uint8 -> forall eff. Ivory eff ()) ->
    Ivory (ProcEffects s t) ()
transmitDiscovery address' Master{..} =
    run $ \transmit -> lookupMac table address' $ \rec -> do
        transmit $ discovery txPreamble
        arrayMap $ \ix ->
            transmit
                =<< deref (rec ~> T.mac ! ix)
        transmit address'

transmitPing ::
    Uint8 ->
    Master n ->
    (Uint8 -> forall eff. Ivory eff ()) ->
    Ivory (ProcEffects s t) ()
transmitPing address' m =
    run $ \transmit -> do
        transmit $ ping txPreamble
        transmit address'

transmitConfirm ::
    Uint8 ->
    Master n ->
    (Uint8 -> forall eff. Ivory eff ()) ->
    Ivory (ProcEffects s t) ()
transmitConfirm address' m =
    run $ \transmit -> do
        transmit $ confirm txPreamble
        transmit address'

run ::
    ((Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s t) ()) ->
    (Uint8 -> forall eff. Ivory eff ()) ->
    Ivory (ProcEffects s t) ()
run tx transmit = do
    crc <- local $ istruct initCRC16
    tx $ \v -> updateCRC16 crc v >> transmit v
    transmit =<< deref (crc ~> msb)
    transmit =<< deref (crc ~> lsb)
