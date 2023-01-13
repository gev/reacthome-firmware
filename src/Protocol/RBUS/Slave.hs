{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NamedFieldPuns     #-}

module Protocol.RBUS.Slave where

import           GHC.TypeNats
import           Include
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RBUS
import           Util.CRC16
import           Util.Data.Buffer
import           Util.Data.Class
import           Util.Data.Record
import           Util.Data.Value


data Slave n = Slave
    { mac     :: Buffer 6 Uint8
    , address :: Value Uint8
    , state   :: Value Uint8
    , phase   :: Value Uint8
    , index   :: Value Uint8
    , size    :: Value Uint8
    , buff    :: Buffer n Uint8
    , crc     :: Record CRC16
    , handle  :: forall eff. Buffer n Uint8 -> Ivory eff ()
    }


rxPreamble :: Preamble
rxPreamble = preambleMaster

txPreamble :: Preamble
txPreamble = preambleSlave


slave :: KnownNat n
      => String
      -> Mac
      -> (forall eff. Buffer n Uint8 -> Ivory eff ())
      -> Slave n
slave name mac handle = Slave
    { mac     = mac
    , address = value  (name <> "_address") broadcastAddress
    , state   = value  (name <> "_state") readyToReceive
    , phase   = value  (name <> "_phase") waitingAddress
    , index   = value  (name <> "_index") 0
    , size    = value  (name <> "_size") 0
    , buff    = buffer (name <> "_message")
    , crc     = record (name <> "_crc") initCRC16
    , handle  = handle
    }


instance Include (Slave n) where
    include (Slave {address, state, index, phase, size, buff, crc}) = do
        include address
        include state
        include phase
        include index
        include size
        include buff
        include crc


receiveData :: KnownNat n => Slave n -> Uint8 -> Ivory eff ()
receiveData = runReceive state
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
                       ==> do setValue state s
                              setValue phase p
                              setValue index 0
                              setValue size  0
                              crc <| msb $ initCRC
                              crc <| lsb $ initCRC
                              updateCRC crc  v



receiveDiscovery :: KnownNat n => Slave n -> Uint8 -> Ivory eff ()
receiveDiscovery = runReceive phase
    [ go waitingData    receiveDiscoveryMac
    , go waitingAddress receiveDiscoveryAddress
    , go waitingMsbCRC  receiveMsbCRC
    , go waitingLsbCRC  receiveDiscoveryLsbCRC
    ]

receiveDiscoveryMac :: Slave n -> Uint8 -> Ivory eff ()
receiveDiscoveryMac (Slave {mac, index, state, phase, crc}) v = do
    i <- getValue index
    m <- getItem mac $ toIx i
    ifte_ (v ==? m)
          (do setValue index $ i + 1
              updateCRC crc v
              when (i ==? getSize mac)
                   (setValue phase waitingAddress)
          )
          (setValue state readyToReceive)

receiveDiscoveryAddress :: KnownNat n => Slave n -> Uint8 -> Ivory eff ()
receiveDiscoveryAddress (Slave {phase, buff, crc}) v = do
    setItem buff 0 v
    updateCRC crc v
    setValue phase waitingMsbCRC

receiveDiscoveryLsbCRC :: KnownNat n => Slave n -> Uint8 -> Ivory eff ()
receiveDiscoveryLsbCRC r@(Slave {address, buff}) =
    receiveLsbCRC r $ setValue address =<< getItem buff 0



receivePing :: KnownNat n => Slave n -> Uint8 -> Ivory eff ()
receivePing = runReceive phase
    [ go waitingAddress $ receiveAddress waitingMsbCRC
    , go waitingMsbCRC    receiveMsbCRC
    , go waitingLsbCRC    receivePingLsbCRC
    ]

receivePingLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receivePingLsbCRC r@(Slave {address}) =
    receiveLsbCRC r $ setValue address broadcastAddress



receiveConfirm :: KnownNat n => Slave n -> Uint8 -> Ivory eff ()
receiveConfirm = runReceive phase
    [ go waitingAddress $ receiveAddress waitingMsbCRC
    , go waitingMsbCRC    receiveMsbCRC
    , go waitingLsbCRC    receiveConfirmLsbCRC
    ]

receiveConfirmLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveConfirmLsbCRC r =
    receiveLsbCRC r undefined



receiveMessage :: KnownNat n => Slave n -> Uint8 -> Ivory eff ()
receiveMessage = runReceive phase
    [ go waitingAddress $ receiveAddress waitingSize
    , go waitingSize      receiveMessageSize
    , go waitingData      receiveMessageData
    , go waitingMsbCRC    receiveMsbCRC
    , go waitingLsbCRC    receiveMessageLsbCRC
    ]

receiveMessageSize :: Slave n -> Uint8 -> Ivory eff ()
receiveMessageSize (Slave {phase, index, size, crc}) v = do
    setValue index 0
    setValue size v
    updateCRC crc v
    setValue phase waitingData

receiveMessageData :: KnownNat n => Slave n -> Uint8 -> Ivory eff ()
receiveMessageData (Slave {phase, index, size, buff, crc}) v = do
    i <- getValue index
    s <- getValue size
    setItem buff (toIx i) v
    setValue index $ i + 1
    updateCRC crc v
    when (i ==? s)
         (setValue phase waitingMsbCRC)

receiveMessageLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveMessageLsbCRC r@(Slave {buff, handle}) =
    receiveLsbCRC r $ handle buff



receiveAddress :: Uint8 -> Slave n -> Uint8 -> Ivory eff ()
receiveAddress p (Slave {address, state, phase, crc}) v = do
    a <- getValue address
    ifte_ (v==? a .|| v ==? broadcastAddress)
          (updateCRC crc v >> setValue phase p)
          (setValue state readyToReceive)

receiveMsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveMsbCRC (Slave {state, phase, crc}) v = do
    updateCRC crc v
    b <- crc |> msb
    ifte_ (b ==? v)
          (setValue phase waitingLsbCRC)
          (setValue state readyToReceive)

receiveLsbCRC :: Slave n -> Ivory eff () -> Uint8 -> Ivory eff ()
receiveLsbCRC (Slave {state, crc}) complete v = do
    updateCRC crc v
    b <- crc |> lsb
    when (b ==? v) complete
    setValue state readyToReceive



go :: a -> b -> (a, b)
go = (,)


runReceive :: (Val v a, IvoryEq a)
           => (r -> v a)
           -> [(a, r -> Uint8 -> Ivory eff ())]
           -> r
           -> Uint8
           -> Ivory eff ()
runReceive f hs r v = do
    p <- getValue . f $ r
    let go (w, h) = w ==? p ==> h r v
    cond_ $ map go hs
