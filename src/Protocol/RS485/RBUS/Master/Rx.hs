module Protocol.RS485.RBUS.Master.Rx (
    receive,
    reset,
) where

import Core.FSM
import Core.Version
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib
import Protocol.RS485.RBUS
import Protocol.RS485.RBUS.Master
import Protocol.RS485.RBUS.Master.MacTable as T
import Util.CRC16

receive :: (KnownNat n) => Master n -> Uint8 -> Ivory (ProcEffects s ()) ()
receive =
    runState
        state
        [ readyToReceive |-> receivePreamble
        , receivingMessage |-> receiveMessage
        , receivingConfirm |-> receiveConfirm
        , receivingDiscovery |-> receiveDiscovery
        , receivingPing |-> receivePing
        ]

receivePreamble :: Master n -> Uint8 -> Ivory eff ()
receivePreamble =
    runInput
        [ discovery rxPreamble |-> start receivingDiscovery waitingMac
        , ping rxPreamble |-> start receivingPing waitingAddress
        , confirm rxPreamble |-> start receivingConfirm waitingAddress
        , message rxPreamble |-> start receivingMessage waitingAddress
        ]

start :: Uint8 -> Uint8 -> Master n -> Uint8 -> Ivory eff ()
start s p Master{..} v = do
    store state s
    store phase p
    store offset 0
    store size 0
    store valid true
    store (crc ~> msb) initCRC
    store (crc ~> lsb) initCRC
    updateCRC16 crc v

receiveDiscovery :: (KnownNat n) => Master n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveDiscovery =
    runState
        phase
        [ waitingMac |-> receiveDiscoveryMac
        , waitingModel |-> receiveDiscoveryModel
        , waitingMajorVersion |-> receiveDiscoveryMajorVersion
        , waitingMinorVersion |-> receiveDiscoveryMinorVersion
        , waitingMsbCRC |-> receiveMsbCRC
        , waitingLsbCRC |-> receiveDiscoveryLsbCRC
        ]

receiveDiscoveryMac :: Master n -> Uint8 -> Ivory eff ()
receiveDiscoveryMac Master{..} v = do
    i <- deref offset
    store (mac ! toIx i) v
    store offset $ i + 1
    i <- deref offset
    updateCRC16 crc v
    when
        (i ==? arrayLen mac)
        (store phase waitingModel)

receiveDiscoveryModel :: Master n -> Uint8 -> Ivory eff ()
receiveDiscoveryModel Master{..} v = do
    store model v
    updateCRC16 crc v
    store phase waitingMajorVersion

receiveDiscoveryMajorVersion :: Master n -> Uint8 -> Ivory eff ()
receiveDiscoveryMajorVersion Master{..} v = do
    store (version ~> major) v
    updateCRC16 crc v
    store phase waitingMinorVersion

receiveDiscoveryMinorVersion :: Master n -> Uint8 -> Ivory eff ()
receiveDiscoveryMinorVersion Master{..} v = do
    store (version ~> minor) v
    updateCRC16 crc v
    store phase waitingMsbCRC

receiveDiscoveryLsbCRC :: Master n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveDiscoveryLsbCRC m@Master{..} =
    receiveLsbCRC m $
        insertMac table mac model version onDiscovery

receivePing :: (KnownNat n) => Master n -> Uint8 -> Ivory (ProcEffects s ()) ()
receivePing =
    runState
        phase
        [ waitingAddress |-> receiveAddress waitingMsbCRC
        , waitingMsbCRC |-> receiveMsbCRC
        , waitingLsbCRC |-> receivePingLsbCRC
        ]

receivePingLsbCRC :: Master n -> Uint8 -> Ivory (ProcEffects s ()) ()
receivePingLsbCRC m@Master{..} = receiveLsbCRC m do
    address' <- deref address
    lookupMac table address' \rec ->
        onPing (rec ~> T.mac) address' (rec ~> T.model) (rec ~> T.version)

receiveConfirm :: (KnownNat n) => Master n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveConfirm =
    runState
        phase
        [ waitingAddress |-> receiveAddress waitingMsbCRC
        , waitingMsbCRC |-> receiveMsbCRC
        , waitingLsbCRC |-> receiveConfirmLsbCRC
        ]

receiveConfirmLsbCRC :: Master n -> Uint8 -> Ivory eff ()
receiveConfirmLsbCRC m@Master{..} = receiveLsbCRC m do
    address' <- deref address
    onConfirm address'

receiveMessage :: (KnownNat n) => Master n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveMessage =
    runState
        phase
        [ waitingAddress |-> receiveAddress waitingTid
        , waitingTid |-> receiveMessageTid
        , waitingSize |-> receiveMessageSize
        , waitingData |-> receiveMessageData
        , waitingMsbCRC |-> receiveMsbCRC
        , waitingLsbCRC |-> receiveMessageLsbCRC
        ]

receiveMessageTid :: Master n -> Uint8 -> Ivory eff ()
receiveMessageTid Master{..} v = do
    store tmp v
    updateCRC16 crc v
    store phase waitingSize

receiveMessageSize :: Master n -> Uint8 -> Ivory eff ()
receiveMessageSize Master{..} v = do
    store size v
    updateCRC16 crc v
    ifte_
        (v ==? 0)
        do store phase waitingMsbCRC
        do store phase waitingData

receiveMessageData :: (KnownNat n) => Master n -> Uint8 -> Ivory eff ()
receiveMessageData Master{..} v = do
    i <- deref offset
    store (buff ! toIx i) v
    store offset $ i + 1
    updateCRC16 crc v
    s <- deref size
    i <- deref offset
    when
        (i ==? s)
        (store phase waitingMsbCRC)

receiveMessageLsbCRC :: (KnownNat n) => Master n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveMessageLsbCRC m@Master{..} v = do
    tmp' <- deref tmp
    size' <- deref size
    address' <- deref address
    let tidRx' = tidRx ! toIx address'
    id <- deref tidRx'
    let complete = do
            store tidRx' $ safeCast tmp'
            lookupMac table address' \rec ->
                onMessage (rec ~> T.mac) address' buff size' $ id /=? safeCast tmp'
    receiveLsbCRC m complete v

receiveAddress :: Uint8 -> Master n -> Uint8 -> Ivory eff ()
receiveAddress p Master{..} v = do
    store phase p
    store address v
    updateCRC16 crc v

receiveMsbCRC :: Master n -> Uint8 -> Ivory eff ()
receiveMsbCRC Master{..} v = do
    msb' <- deref $ crc ~> msb
    when (msb' /=? v) $ store valid false
    store phase waitingLsbCRC

receiveLsbCRC :: Master n -> Ivory eff () -> Uint8 -> Ivory eff ()
receiveLsbCRC m@Master{..} complete v = do
    onReceive
    valid' <- deref valid
    lsb' <- deref $ crc ~> lsb
    when (valid' .&& lsb' ==? v) complete
    reset m

reset :: Master n -> Ivory eff ()
reset Master{..} = store state readyToReceive
