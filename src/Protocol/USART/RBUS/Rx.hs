{-# LANGUAGE RecordWildCards #-}

module Protocol.USART.RBUS.Rx (receive) where

import           Core.FSM
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.USART.RBUS
import           Util.CRC16



receive :: KnownNat n => RBUS n -> Uint8 -> Ivory (ProcEffects s ()) ()
receive = runState state
    [ readyToReceive     |-> receivePreamble
    , receivingMessage   |-> receiveMessage
    ]



receivePreamble :: RBUS n -> Uint8 -> Ivory eff ()
receivePreamble = runInput preamble
    [ message   |-> start receivingMessage waitingTid
    ]

start :: Uint8 -> Uint8 -> RBUS n -> Uint8 -> Ivory eff ()
start s p (RBUS {..}) v = do
    store state s
    store phase p
    store index 0
    store size  0
    store (crc ~> msb) initCRC
    store (crc ~> lsb) initCRC
    updateCRC16 crc v



receiveMessage :: KnownNat n => RBUS n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveMessage = runState phase
    [ waitingTid       |-> receiveMessageTid
    , waitingSize      |-> receiveMessageSize
    , waitingData      |-> receiveMessageData
    , waitingMsbCRC    |-> receiveMsbCRC
    , waitingLsbCRC    |-> receiveMessageLsbCRC
    ]

receiveMessageTid :: RBUS n -> Uint8 -> Ivory eff ()
receiveMessageTid (RBUS {..}) v = do
    store tmp v
    updateCRC16 crc v
    store phase waitingSize

receiveMessageSize :: RBUS n -> Uint8 -> Ivory eff ()
receiveMessageSize (RBUS {..}) v = do
    store size v
    updateCRC16 crc v
    store phase waitingData

receiveMessageData :: KnownNat n => RBUS n -> Uint8 -> Ivory eff ()
receiveMessageData (RBUS {..}) v = do
    i <- deref index
    s <- deref size
    store (buff ! toIx i) v
    store index $ i + 1
    updateCRC16 crc v
    i <- deref index
    when (i ==? s)
         (store phase waitingMsbCRC)

receiveMessageLsbCRC :: RBUS n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveMessageLsbCRC r@(RBUS {..}) v = do
    tmp'   <- deref tmp
    size'  <- deref size
    tidRx' <- deref tidRx
    let complete = do store tidRx $ safeCast tmp'
                      onMessage buff size' $ tidRx' /=? safeCast tmp'
    receiveLsbCRC r complete v



receiveMsbCRC :: RBUS n -> Uint8 -> Ivory eff ()
receiveMsbCRC (RBUS {..}) v = do
    b <- deref $ crc ~> msb
    ifte_ (b ==? v)
          (store phase waitingLsbCRC)
          (store state readyToReceive)

receiveLsbCRC :: RBUS n -> Ivory eff () -> Uint8 -> Ivory eff ()
receiveLsbCRC (RBUS {..}) complete v = do
    b <- deref $ crc ~> lsb
    when (b ==? v) complete
    store state readyToReceive
