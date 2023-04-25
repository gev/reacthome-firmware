{-# LANGUAGE RecordWildCards #-}

module Protocol.UART.RBUS.Rx
    ( receive
    , reset
    ) where

import           Core.FSM
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.UART.RBUS
import           Util.CRC16



receive :: KnownNat n => RBUS n -> Uint8 -> Ivory (ProcEffects s ()) ()
receive = runState state
    [ readyToReceive     |-> receivePreamble
    , receivingMessage   |-> receiveMessage
    ]



receivePreamble :: RBUS n -> Uint8 -> Ivory eff ()
receivePreamble = runInput preamble
    [ message   |-> start receivingMessage waitingSize
    ]

start :: Uint8 -> Uint8 -> RBUS n -> Uint8 -> Ivory eff ()
start s p RBUS{..} v = do
    store state   s
    store phase   p
    store offset  0
    store size    0
    store valid   true
    store (crc ~> msb) initCRC
    store (crc ~> lsb) initCRC
    updateCRC16 crc v



receiveMessage :: KnownNat n => RBUS n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveMessage = runState phase
    [ waitingSize      |-> receiveMessageSize
    , waitingData      |-> receiveMessageData
    , waitingMsbCRC    |-> receiveMsbCRC
    , waitingLsbCRC    |-> receiveMessageLsbCRC
    ]

receiveMessageSize :: RBUS n -> Uint8 -> Ivory eff ()
receiveMessageSize RBUS{..} v = do
    store size v
    updateCRC16 crc v
    store phase waitingData

receiveMessageData :: KnownNat n => RBUS n -> Uint8 -> Ivory eff ()
receiveMessageData RBUS{..} v = do
    i <- deref offset
    s <- deref size
    store (buff ! toIx i) v
    store offset $ i + 1
    updateCRC16 crc v
    i <- deref offset
    when (i ==? s)
         (store phase waitingMsbCRC)

receiveMessageLsbCRC :: RBUS n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveMessageLsbCRC r@RBUS{..} v = do
    tmp'   <- deref tmp
    size'  <- deref size
    let complete = onMessage buff size'
    receiveLsbCRC r complete v



receiveMsbCRC :: RBUS n -> Uint8 -> Ivory eff ()
receiveMsbCRC RBUS{..} v = do
    msb' <- deref $ crc ~> msb
    when (msb' /=? v) $ store valid false
    store phase waitingLsbCRC

receiveLsbCRC :: RBUS n -> Ivory eff () -> Uint8 -> Ivory eff ()
receiveLsbCRC r@RBUS{..} complete v = do
    valid' <- deref valid
    lsb'   <- deref $ crc ~> lsb
    when (valid' .&& lsb' ==? v) complete
    reset r



reset :: RBUS n -> Ivory eff ()
reset RBUS{..} = store state readyToReceive
