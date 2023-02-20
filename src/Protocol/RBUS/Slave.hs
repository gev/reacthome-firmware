{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Protocol.RBUS.Slave where

import           Control.Monad   ((<=<))
import           Core.Include
import           Core.Initialize
import           Core.Version    (Version, major, minor)
import           Data.Buffer
import           Data.Class
import           Data.Record
import           Data.Value
import           GHC.TypeNats
import           Interface.Mac   (Mac)
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RBUS
import           Util.CRC16



data Slave n = Slave
    { name     :: String
    , mac      :: Buffer  6 Uint8
    , model    :: Value     Uint8
    , version  :: Version
    , address  :: Value     Uint8
    , state    :: Value     Uint8
    , phase    :: Value     Uint8
    , index    :: Value     Uint8
    , size     :: Value     Uint8
    , buff     :: Buffer  n Uint8
    , buffConf :: Buffer  4 Uint8
    , buffPing :: Buffer  4 Uint8
    , buffDisc :: Buffer 12 Uint8
    , tid      :: Value     Uint8
    , crc      :: Record    CRC16
    , handle   :: Buffer n Uint8 -> Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }


rxPreamble :: Preamble
rxPreamble = preambleMaster

txPreamble :: Preamble
txPreamble = preambleSlave


slave :: KnownNat n
      => String
      -> Buffer 6 Uint8
      -> Value Uint8
      -> Version
      -> (Buffer n Uint8 -> Uint8 -> forall s. Ivory (ProcEffects s ()) ())
      -> Slave n
slave n mac model version handle = Slave
    { name     = name
    , mac      = mac
    , model    = model
    , version  = version
    , address  = value      (name <> "_address")      broadcastAddress
    , state    = value      (name <> "_state")        readyToReceive
    , phase    = value      (name <> "_phase")        waitingAddress
    , index    = value      (name <> "_index")        0
    , size     = value      (name <> "_size")         0
    , buff     = buffer     (name <> "_message")
    , buffConf = buffer     (name <> "_confirm_tx")
    , buffPing = buffer     (name <> "_ping_tx")
    , buffDisc = buffer     (name <> "_disc_tx")
    , tid      = value      (name <> "_tid")          0
    , crc      = record     (name <> "_crc")          initCRC16
    , handle   = handle
    } where name = "protocol_" <> n


instance Include (Slave n) where
    include (Slave {address, state, index, phase, size, buff, buffConf, buffPing, buffDisc, tid, crc}) = do
        inclCRC16
        include address
        include state
        include phase
        include index
        include size
        include buff
        include buffConf
        include buffPing
        include buffDisc
        include tid
        include crc


instance Initialize (Slave n) where
    initialize s = ($ s) <$> [initDisc, initConf, initPing]

initDisc :: Slave n -> Def('[] :-> ())
initDisc (Slave {name, mac, model, version, buffDisc}) =
    proc (name <> "_init_disc_tx") $ body $ do
        setItem buffDisc 0 $ discovery txPreamble
        arrayCopy (getBuffer buffDisc) (getBuffer mac) 1 (getSize mac)
        setItem buffDisc 7 =<< getValue model
        setItem buffDisc 8 =<< version |> major
        setItem buffDisc 9 =<< version |> minor
        calcCRC16 buffDisc

initConf :: Slave n -> Def('[] :-> ())
initConf (Slave {name, address, buffConf}) =
    proc (name <> "_init_conf_tx") $ body $ do
        setItem buffConf 0 $ confirm txPreamble
        setItem buffConf 1 =<< getValue address
        calcCRC16 buffConf

initPing :: Slave n -> Def('[] :-> ())
initPing (Slave {name, address, buffPing}) =
    proc (name <> "_init_ping_tx") $ body $ do
        setItem buffPing 0 $ ping txPreamble
        setItem buffPing 1 =<< getValue address
        calcCRC16 buffPing

calcCRC16 :: KnownNat n => Buffer n Uint8 -> Ivory (ProcEffects s ()) ()
calcCRC16 buff = do
    let size = getSize buff :: Uint16
    let s_2 = toIx $ size - 2
    let s_1 = toIx $ size - 1
    crc <- local $ istruct initCRC16
    for s_2 $ updateCRC16 crc <=< getItem buff
    setItem buff s_2 =<< deref (crc ~> msb)
    setItem buff s_1 =<< deref (crc ~> lsb)


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
                       ==> do setValue state s
                              setValue phase p
                              setValue index 0
                              setValue size  0
                              crc <| msb $ initCRC
                              crc <| lsb $ initCRC
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
    i <- getValue index
    m <- getItem mac $ toIx i
    ifte_ (v ==? m)
          (do setValue index $ i + 1
              i <- getValue index
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
receiveDiscoveryLsbCRC s@(Slave {address, buff}) = do
    receiveLsbCRC s $ do
        setValue address =<< getItem buff 0
        call_ $ initConf s
        call_ $ initPing s



receivePing :: KnownNat n => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receivePing = runReceive phase
    [ go waitingAddress $ receiveAddress waitingMsbCRC
    , go waitingMsbCRC    receiveMsbCRC
    , go waitingLsbCRC    receivePingLsbCRC
    ]

receivePingLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receivePingLsbCRC r@(Slave {address}) =
    receiveLsbCRC r $ setValue address broadcastAddress



receiveConfirm :: KnownNat n => Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveConfirm = runReceive phase
    [ go waitingAddress $ receiveAddress waitingMsbCRC
    , go waitingMsbCRC    receiveMsbCRC
    , go waitingLsbCRC    receiveConfirmLsbCRC
    ]

receiveConfirmLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveConfirmLsbCRC r =
    receiveLsbCRC r $ pure ()



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
receiveMessageTid (Slave {phase, crc}) v = do
    updateCRC crc v
    setValue phase waitingSize

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
    i <- getValue index
    when (i ==? s)
         (setValue phase waitingMsbCRC)

receiveMessageLsbCRC :: Slave n -> Uint8 -> Ivory (ProcEffects s ()) ()
receiveMessageLsbCRC r@(Slave {buff, size, handle}) v = do
    s <- getValue size
    receiveLsbCRC r (handle buff s) v



receiveAddress :: Uint8 -> Slave n -> Uint8 -> Ivory eff ()
receiveAddress p (Slave {address, state, phase, crc}) v = do
    a <- getValue address
    ifte_ (v==? a .|| v ==? broadcastAddress)
          (updateCRC crc v >> setValue phase p)
          (setValue state readyToReceive)

receiveMsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveMsbCRC (Slave {state, phase, crc}) v = do
    b <- crc |> msb
    ifte_ (b ==? v)
          (setValue phase waitingLsbCRC)
          (setValue state readyToReceive)

receiveLsbCRC :: Slave n -> Ivory eff () -> Uint8 -> Ivory eff ()
receiveLsbCRC (Slave {state, crc}) complete v = do
    b <- crc |> lsb
    when (b ==? v) complete
    setValue state readyToReceive


transmitMessage :: KnownNat l
                => Slave n
                -> Buffer l Uint8
                -> (Uint8 -> forall eff. Ivory eff ())
                -> Ivory (ProcEffects s ()) ()
transmitMessage (Slave{address, tid}) payload transmit = do
    crc <- local $ istruct initCRC16
    let t :: Uint8 -> Ivory eff ()
        t v = updateCRC16 crc v >> transmit v
    t $ message txPreamble
    t =<< getValue address
    id <- getValue tid
    t id
    setValue tid $ id + 1
    t $ getSize payload
    arrayMap $ t <=< getItem payload
    transmit =<< deref (crc~>msb)
    transmit =<< deref (crc~>lsb)


hasAddress :: Slave n -> Ivory eff IBool
hasAddress (Slave {address}) = do
    a <- getValue address
    pure $ a /=? broadcastAddress
