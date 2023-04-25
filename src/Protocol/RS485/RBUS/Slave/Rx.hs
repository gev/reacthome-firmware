{-# LANGUAGE RecordWildCards #-}

module Protocol.RS485.RBUS.Slave.Rx (receive) where

import           Core.FSM
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RS485.RBUS
import           Protocol.RS485.RBUS.Slave
import           Util.CRC16



receive :: KnownNat n => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receive = runState state
    [ readyToReceive     |-> receivePreamble
    , receivingMessage   |-> receiveMessage
    , receivingConfirm   |-> receiveConfirm
    , receivingDiscovery |-> receiveDiscovery
    , receivingPing      |-> receivePing
    ]



receivePreamble :: Slave n -> Uint8 -> Ivory eff ()
receivePreamble = runInput rxPreamble
    [ discovery |-> reset receivingDiscovery waitingMac
    , ping      |-> reset receivingPing      waitingAddress
    , confirm   |-> reset receivingConfirm   waitingAddress
    , message   |-> reset receivingMessage   waitingAddress
    ]

reset :: Uint8 -> Uint8 -> Slave n -> Uint8 -> Ivory eff ()
reset s p Slave{..} v = do
    store state   s
    store phase   p
    store offset  0
    store size    0
    store (crc ~> msb) initCRC
    store (crc ~> lsb) initCRC
    updateCRC16 crc v



receiveDiscovery :: KnownNat n => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveDiscovery = runState phase
    [ waitingMac     |-> receiveDiscoveryMac
    , waitingAddress |-> receiveDiscoveryAddress
    , waitingMsbCRC  |-> receiveMsbCRC
    , waitingLsbCRC  |-> receiveDiscoveryLsbCRC
    ]

receiveDiscoveryMac :: Slave n -> Uint8 -> Ivory eff ()
receiveDiscoveryMac Slave{..} v = do
    i <- deref offset
    m <- deref $ mac ! toIx i
    ifte_ (v ==? m)
          (do store offset $ i + 1
              i <- deref offset
              updateCRC16 crc v
              when (i ==? arrayLen mac)
                   (store phase waitingAddress)
          )
          (store valid false)

receiveDiscoveryAddress :: Slave n -> Uint8 -> Ivory eff ()
receiveDiscoveryAddress Slave{..} v = do
    store tmp v
    updateCRC16 crc v
    store phase waitingMsbCRC

receiveDiscoveryLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveDiscoveryLsbCRC s@Slave{..} = receiveLsbCRC s $ do
    store address =<< deref tmp
    call_ $ initConf s
    call_ $ initPing s
    onDiscovery



receivePing :: KnownNat n => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receivePing = runState phase
    [ waitingAddress   |-> receiveAddress waitingMsbCRC
    , waitingMsbCRC    |-> receiveMsbCRC
    , waitingLsbCRC    |-> receivePingLsbCRC
    ]

receivePingLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receivePingLsbCRC r@Slave{..} =
    receiveLsbCRC r $ store address broadcastAddress



receiveConfirm :: KnownNat n => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveConfirm = runState phase
    [ waitingAddress   |-> receiveAddress waitingMsbCRC
    , waitingMsbCRC    |-> receiveMsbCRC
    , waitingLsbCRC    |-> receiveConfirmLsbCRC
    ]

receiveConfirmLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveConfirmLsbCRC r =
    receiveLsbCRC r $ onConfirm r



receiveMessage :: KnownNat n => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveMessage = runState phase
    [ waitingAddress   |-> receiveAddress waitingTid
    , waitingTid       |-> receiveMessageTid
    , waitingSize      |-> receiveMessageSize
    , waitingData      |-> receiveMessageData
    , waitingMsbCRC    |-> receiveMsbCRC
    , waitingLsbCRC    |-> receiveMessageLsbCRC
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
    store phase waitingData

receiveMessageData :: KnownNat n => Slave n -> Uint8 -> Ivory eff ()
receiveMessageData Slave{..} v = do
    i <- deref offset
    s <- deref size
    store (buff ! toIx i) v
    store offset $ i + 1
    updateCRC16 crc v
    i <- deref offset
    when (i ==? s)
         (store phase waitingMsbCRC)

receiveMessageLsbCRC :: Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveMessageLsbCRC r@Slave{..} v = do
    tmp'   <- deref tmp
    size'  <- deref size
    tidRx' <- deref tidRx
    let complete = do store tidRx $ safeCast tmp'
                      onMessage buff size' $ tidRx' /=? safeCast tmp'
    receiveLsbCRC r complete v



receiveAddress :: Uint8 -> Slave n -> Uint8 -> Ivory eff ()
receiveAddress p Slave{..} v = do
    a <- deref address
    ifte_ (v==? a .|| v ==? broadcastAddress)
          (updateCRC16 crc v >> store phase p)
          (store valid false)

receiveMsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveMsbCRC Slave{..} v = do
    msb' <- deref $ crc ~> msb
    when (msb' /=? v) $ store valid false
    store phase waitingLsbCRC

receiveLsbCRC :: Slave n -> Ivory eff () -> Uint8 -> Ivory eff ()
receiveLsbCRC Slave{..} complete v = do
    valid' <- deref valid
    lsb'   <- deref $ crc ~> lsb
    when (valid' .&& lsb' ==? v) complete
    store state readyToReceive
