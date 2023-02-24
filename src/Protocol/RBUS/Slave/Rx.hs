{-# LANGUAGE NamedFieldPuns #-}

module Protocol.RBUS.Slave.Rx (receive) where

import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RBUS
import           Protocol.RBUS.Slave
import           Util.CRC16


receive :: KnownNat n => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receive = runReceive state
    [ go readyToReceive     receivePreamble
    , go receivingMessage   receiveMessage
    , go receivingConfirm   receiveConfirm
    , go receivingDiscovery receiveDiscovery
    , go receivingPing      receivePing
    ]


receivePreamble :: Slave n -> Uint8 -> Ivory eff ()
receivePreamble (Slave {state, phase, index, size, crc}) v =
    cond_ [ go discovery receivingDiscovery waitingData
          , go ping      receivingPing      waitingAddress
          , go confirm   receivingConfirm   waitingAddress
          , go message   receivingMessage   waitingAddress
          ]
    where go f s p = v ==? f rxPreamble
                       ==> do store (addrOf state) s
                              store (addrOf phase) p
                              store (addrOf index) 0
                              store (addrOf size ) 0
                              store (addrOf crc ~> msb) initCRC
                              store (addrOf crc ~> lsb) initCRC
                              updateCRC crc  v



receiveDiscovery :: KnownNat n => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveDiscovery = runReceive phase
    [ go waitingData    receiveDiscoveryMac
    , go waitingAddress receiveDiscoveryAddress
    , go waitingMsbCRC  receiveMsbCRC
    , go waitingLsbCRC  receiveDiscoveryLsbCRC
    ]

receiveDiscoveryMac :: Slave n -> Uint8 -> Ivory eff ()
receiveDiscoveryMac (Slave {mac, index, state, phase, crc}) v = do
    let mac'   = addrOf mac
    let index' = addrOf index
    i <- deref index'
    m <- deref $ mac' ! toIx i
    ifte_ (v ==? m)
          (do store index' $ i + 1
              i <- deref index'
              updateCRC crc v
              when (i ==? arrayLen mac')
                   (store (addrOf phase) waitingAddress)
          )
          (store (addrOf state) readyToReceive)

receiveDiscoveryAddress :: Slave n -> Uint8 -> Ivory eff ()
receiveDiscoveryAddress (Slave {phase, tmp, crc}) v = do
    store (addrOf tmp) v
    updateCRC crc v
    store (addrOf phase) waitingMsbCRC

receiveDiscoveryLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveDiscoveryLsbCRC s@(Slave {address, tmp, onDiscovery}) = do
    receiveLsbCRC s $ do
        store (addrOf address) =<< deref (addrOf tmp)
        call_ $ initConf s
        call_ $ initPing s
        onDiscovery



receivePing :: KnownNat n => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receivePing = runReceive phase
    [ go waitingAddress $ receiveAddress waitingMsbCRC
    , go waitingMsbCRC    receiveMsbCRC
    , go waitingLsbCRC    receivePingLsbCRC
    ]

receivePingLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receivePingLsbCRC r@(Slave {address}) =
    receiveLsbCRC r $ store (addrOf address) broadcastAddress



receiveConfirm :: KnownNat n => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveConfirm = runReceive phase
    [ go waitingAddress $ receiveAddress waitingMsbCRC
    , go waitingMsbCRC    receiveMsbCRC
    , go waitingLsbCRC    receiveConfirmLsbCRC
    ]

receiveConfirmLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveConfirmLsbCRC r =
    receiveLsbCRC r $ onConfirm r



receiveMessage :: KnownNat n => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveMessage = runReceive phase
    [ go waitingAddress $ receiveAddress waitingTid
    , go waitingTid       receiveMessageTid
    , go waitingSize      receiveMessageSize
    , go waitingData      receiveMessageData
    , go waitingMsbCRC    receiveMsbCRC
    , go waitingLsbCRC    receiveMessageLsbCRC
    ]

receiveMessageTid :: Slave n -> Uint8 -> Ivory eff ()
receiveMessageTid (Slave {phase, crc, tidRx, tmp}) v = do
    store (addrOf tmp) v
    updateCRC crc v
    store (addrOf phase) waitingSize

receiveMessageSize :: Slave n -> Uint8 -> Ivory eff ()
receiveMessageSize (Slave {phase, index, size, crc}) v = do
    store (addrOf size) v
    updateCRC crc v
    store (addrOf phase) waitingData

receiveMessageData :: KnownNat n => Slave n -> Uint8 -> Ivory eff ()
receiveMessageData (Slave {phase, index, size, buff, crc}) v = do
    let index' = addrOf index
    i <- deref index'
    s <- deref $ addrOf size
    store (addrOf buff ! toIx i) v
    store index' $ i + 1
    updateCRC crc v
    i <- deref index'
    when (i ==? s)
         (store (addrOf phase) waitingMsbCRC)

receiveMessageLsbCRC :: Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveMessageLsbCRC r@(Slave {buff, size, onMessage, tidRx, tmp}) v = do
    tmp'   <- deref $ addrOf tmp
    size'  <- deref $ addrOf size
    tidRx' <- deref $ addrOf tidRx
    let complete = do store (addrOf tidRx) $ safeCast tmp'
                      onMessage buff size' $ tidRx' /=? safeCast tmp'
    receiveLsbCRC r complete v



receiveAddress :: Uint8 -> Slave n -> Uint8 -> Ivory eff ()
receiveAddress p (Slave {address, state, phase, crc}) v = do
    a <- deref $ addrOf address
    ifte_ (v==? a .|| v ==? broadcastAddress)
          (updateCRC crc v >> store (addrOf phase) p)
          (store (addrOf state) readyToReceive)

receiveMsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveMsbCRC (Slave {state, phase, crc}) v = do
    b <- deref $ addrOf crc ~> msb
    ifte_ (b ==? v)
          (store (addrOf phase) waitingLsbCRC)
          (store (addrOf state) readyToReceive)

receiveLsbCRC :: Slave n -> Ivory eff () -> Uint8 -> Ivory eff ()
receiveLsbCRC (Slave {state, crc}) complete v = do
    b <- deref $ addrOf crc ~> lsb
    when (b ==? v) complete
    store (addrOf state) readyToReceive
