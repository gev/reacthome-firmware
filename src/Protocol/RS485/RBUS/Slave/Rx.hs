{-# LANGUAGE RecordWildCards #-}

module Protocol.RS485.RBUS.Slave.Rx (
    receive,
    reset,
) where

import Core.FSM
import GHC.TypeNats
import Ivory.Language
import Ivory.Stdlib
import Protocol.RS485.RBUS
import Protocol.RS485.RBUS.Slave
import Util.CRC16

receive :: (KnownNat n) => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receive =
    runState
        state
        [ readyToReceive |-> receivePreamble
        , receivingMessage |-> receiveMessage
        , receivingConfirm |-> receiveConfirm
        , receivingDiscovery |-> receiveDiscovery
        , receivingPing |-> receivePing
        , skippingAll |-> skipAll
        , skippingMsg |-> skipMsg
        ]

receivePreamble :: Slave n -> Uint8 -> Ivory eff ()
receivePreamble = do
    runInput
        [ discovery rxPreamble |-> start receivingDiscovery waitingMac
        , ping rxPreamble |-> start receivingPing waitingAddress
        , confirm rxPreamble |-> start receivingConfirm waitingAddress
        , message rxPreamble |-> start receivingMessage waitingAddress
        , discovery txPreamble |-> start skippingAll 11
        , ping txPreamble |-> start skippingAll 3
        , confirm txPreamble |-> start skippingAll 3
        , message txPreamble |-> start skippingMsg 2
        ]

start :: Uint8 -> Uint8 -> Slave n -> Uint8 -> Ivory eff ()
start s p Slave{..} v = do
    store state s
    store phase p
    store offset 0
    store size 0
    store valid true
    store (crc ~> msb) initCRC
    store (crc ~> lsb) initCRC
    updateCRC16 crc v

receiveDiscovery :: (KnownNat n) => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveDiscovery =
    runState
        phase
        [ waitingMac |-> receiveDiscoveryMac
        , waitingAddress |-> receiveDiscoveryAddress
        , waitingMsbCRC |-> receiveMsbCRC
        , waitingLsbCRC |-> receiveDiscoveryLsbCRC
        ]

receiveDiscoveryMac :: Slave n -> Uint8 -> Ivory eff ()
receiveDiscoveryMac Slave{..} v = do
    i <- deref offset
    m <- deref $ mac ! toIx i
    when
        (v /=? m)
        (store valid false)
    updateCRC16 crc v
    store offset $ i + 1
    i <- deref offset
    when
        (i ==? arrayLen mac)
        (store phase waitingAddress)

receiveDiscoveryAddress :: Slave n -> Uint8 -> Ivory eff ()
receiveDiscoveryAddress Slave{..} v = do
    store tmp v
    updateCRC16 crc v
    store phase waitingMsbCRC

receiveDiscoveryLsbCRC :: Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveDiscoveryLsbCRC s@Slave{..} = receiveLsbCRC s $ do
    store address =<< deref tmp
    call_ initConf
    call_ initPing
    onDiscovery

receivePing :: (KnownNat n) => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receivePing =
    runState
        phase
        [ waitingAddress |-> receiveAddress waitingMsbCRC
        , waitingMsbCRC |-> receiveMsbCRC
        , waitingLsbCRC |-> receivePingLsbCRC
        ]

receivePingLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receivePingLsbCRC s@Slave{..} =
    receiveLsbCRC s $ store address broadcastAddress

receiveConfirm :: (KnownNat n) => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveConfirm =
    runState
        phase
        [ waitingAddress |-> receiveAddress waitingMsbCRC
        , waitingMsbCRC |-> receiveMsbCRC
        , waitingLsbCRC |-> receiveConfirmLsbCRC
        ]

receiveConfirmLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveConfirmLsbCRC s =
    receiveLsbCRC s $ onConfirm s

receiveMessage :: (KnownNat n) => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
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

receiveMessageTid :: Slave n -> Uint8 -> Ivory eff ()
receiveMessageTid Slave{..} v = do
    store tmp v
    updateCRC16 crc v
    store phase waitingSize

receiveMessageSize :: Slave n -> Uint8 -> Ivory eff ()
receiveMessageSize Slave{..} v = do
    store size v
    updateCRC16 crc v
    ifte_
        (v ==? 0)
        (store phase waitingMsbCRC)
        (store phase waitingData)

receiveMessageData :: (KnownNat n) => Slave n -> Uint8 -> Ivory eff ()
receiveMessageData Slave{..} v = do
    i <- deref offset
    store (buff ! toIx i) v
    store offset $ i + 1
    updateCRC16 crc v
    s <- deref size
    i <- deref offset
    when
        (i ==? s)
        (store phase waitingMsbCRC)

receiveMessageLsbCRC :: Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveMessageLsbCRC s@Slave{..} = receiveLsbCRC s $ do
    tmp' <- safeCast <$> deref tmp
    size' <- deref size
    tidRx' <- deref tidRx
    onMessage buff size' $ tidRx' /=? tmp'
    store tidRx tmp'

receiveAddress :: Uint8 -> Slave n -> Uint8 -> Ivory eff ()
receiveAddress p Slave{..} v = do
    a <- deref address
    updateCRC16 crc v
    when (v /=? a .&& v /=? broadcastAddress) $ store valid false
    store phase p

receiveMsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveMsbCRC Slave{..} v = do
    msb' <- deref $ crc ~> msb
    when (msb' /=? v) $ store valid false
    store phase waitingLsbCRC

receiveLsbCRC :: Slave n -> Ivory eff () -> Uint8 -> Ivory eff ()
receiveLsbCRC s@Slave{..} complete v = do
    onReceive
    valid' <- deref valid
    lsb' <- deref $ crc ~> lsb
    when (valid' .&& lsb' ==? v) complete
    reset s

skipAll :: Slave n -> Uint8 -> Ivory eff ()
skipAll s@Slave{..} _ = do
    phase' <- deref phase
    ifte_
        (phase' >? 1)
        (store phase $ phase' - 1)
        (reset s)

skipMsg :: Slave n -> Uint8 -> Ivory eff ()
skipMsg s@Slave{..} v = do
    phase' <- deref phase
    ifte_
        (phase' >? 1)
        (store phase $ phase' - 1)
        ( do
            store state skippingAll
            store phase $ v + 2
        )

reset :: Slave n -> Ivory eff ()
reset Slave{..} = store state readyToReceive
