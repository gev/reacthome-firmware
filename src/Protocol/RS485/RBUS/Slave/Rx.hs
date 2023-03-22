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
receive = runFSM state
    [ readyToReceive     |-> receivePreamble
    , receivingMessage   |-> receiveMessage
    , receivingConfirm   |-> receiveConfirm
    , receivingDiscovery |-> receiveDiscovery
    , receivingPing      |-> receivePing
    ]


receivePreamble :: Slave n -> Uint8 -> Ivory eff ()
receivePreamble (Slave {..}) v =
    cond_ [ go discovery receivingDiscovery waitingData
          , go ping      receivingPing      waitingAddress
          , go confirm   receivingConfirm   waitingAddress
          , go message   receivingMessage   waitingAddress
          ]
    where go f s p = v ==? f rxPreamble
                       ==> do store state s
                              store phase p
                              store index 0
                              store size 0
                              store (crc ~> msb) initCRC
                              store (crc ~> lsb) initCRC
                              updateCRC crc  v



receiveDiscovery :: KnownNat n => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveDiscovery = runFSM phase
    [ waitingData    |-> receiveDiscoveryMac
    , waitingAddress |-> receiveDiscoveryAddress
    , waitingMsbCRC  |-> receiveMsbCRC
    , waitingLsbCRC  |-> receiveDiscoveryLsbCRC
    ]

receiveDiscoveryMac :: Slave n -> Uint8 -> Ivory eff ()
receiveDiscoveryMac (Slave {..}) v = do
    let mac'   =  mac
    let index' =  index
    i <- deref index'
    m <- deref $ mac' ! toIx i
    ifte_ (v ==? m)
          (do store index' $ i + 1
              i <- deref index'
              updateCRC crc v
              when (i ==? arrayLen mac')
                   (store phase waitingAddress)
          )
          (store state readyToReceive)

receiveDiscoveryAddress :: Slave n -> Uint8 -> Ivory eff ()
receiveDiscoveryAddress (Slave {..}) v = do
    store tmp v
    updateCRC crc v
    store phase waitingMsbCRC

receiveDiscoveryLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveDiscoveryLsbCRC s@(Slave {..}) = receiveLsbCRC s $ do
    store address =<< deref tmp
    call_ $ initConf s
    call_ $ initPing s
    onDiscovery



receivePing :: KnownNat n => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receivePing = runFSM phase
    [ waitingAddress   |-> receiveAddress waitingMsbCRC
    , waitingMsbCRC    |-> receiveMsbCRC
    , waitingLsbCRC    |-> receivePingLsbCRC
    ]

receivePingLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receivePingLsbCRC r@(Slave {..}) =
    receiveLsbCRC r $ store address broadcastAddress



receiveConfirm :: KnownNat n => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveConfirm = runFSM phase
    [ waitingAddress   |-> receiveAddress waitingMsbCRC
    , waitingMsbCRC    |-> receiveMsbCRC
    , waitingLsbCRC    |-> receiveConfirmLsbCRC
    ]

receiveConfirmLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveConfirmLsbCRC r =
    receiveLsbCRC r $ onConfirm r



receiveMessage :: KnownNat n => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveMessage = runFSM phase
    [ waitingAddress   |-> receiveAddress waitingTid
    , waitingTid       |-> receiveMessageTid
    , waitingSize      |-> receiveMessageSize
    , waitingData      |-> receiveMessageData
    , waitingMsbCRC    |-> receiveMsbCRC
    , waitingLsbCRC    |-> receiveMessageLsbCRC
    ]

receiveMessageTid :: Slave n -> Uint8 -> Ivory eff ()
receiveMessageTid (Slave {..}) v = do
    store tmp v
    updateCRC crc v
    store phase waitingSize

receiveMessageSize :: Slave n -> Uint8 -> Ivory eff ()
receiveMessageSize (Slave {..}) v = do
    store size v
    updateCRC crc v
    store phase waitingData

receiveMessageData :: KnownNat n => Slave n -> Uint8 -> Ivory eff ()
receiveMessageData (Slave {..}) v = do
    i <- deref index
    s <- deref size
    store (buff ! toIx i) v
    store index $ i + 1
    updateCRC crc v
    i <- deref index
    when (i ==? s)
         (store phase waitingMsbCRC)

receiveMessageLsbCRC :: Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveMessageLsbCRC r@(Slave {..}) v = do
    tmp'   <- deref tmp
    size'  <- deref size
    tidRx' <- deref tidRx
    let complete = do store tidRx $ safeCast tmp'
                      onMessage buff size' $ tidRx' /=? safeCast tmp'
    receiveLsbCRC r complete v



receiveAddress :: Uint8 -> Slave n -> Uint8 -> Ivory eff ()
receiveAddress p (Slave {..}) v = do
    a <- deref address
    ifte_ (v==? a .|| v ==? broadcastAddress)
          (updateCRC crc v >> store phase p)
          (store state readyToReceive)

receiveMsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveMsbCRC (Slave {..}) v = do
    b <- deref $ crc ~> msb
    ifte_ (b ==? v)
          (store phase waitingLsbCRC)
          (store state readyToReceive)

receiveLsbCRC :: Slave n -> Ivory eff () -> Uint8 -> Ivory eff ()
receiveLsbCRC (Slave {..}) complete v = do
    b <- deref $ crc ~> lsb
    when (b ==? v) complete
    store state readyToReceive
