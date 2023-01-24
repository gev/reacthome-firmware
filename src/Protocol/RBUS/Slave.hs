{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}
{-# LANGUAGE TypeOperators    #-}

module Protocol.RBUS.Slave where

import           Control.Monad    ((<=<))
import           GHC.TypeNats
import           Include
import           Initialize
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RBUS
import           Util.CRC16
import           Util.Data.Buffer
import           Util.Data.Class
import           Util.Data.Record
import           Util.Data.Value
import           Util.Version



data Slave n = Slave
    { name     :: String
    , mac      :: Buffer 6 Uint8
    , hwType   :: Value Uint8
    , version  :: Record Version
    , address  :: Value Uint8
    , state    :: Value Uint8
    , phase    :: Value Uint8
    , index    :: Value Uint8
    , size     :: Value Uint8
    , buff     :: Buffer n  Uint8
    , buffConf :: Buffer 4  Uint8
    , buffPing :: Buffer 4  Uint8
    , buffDisc :: Buffer 12 Uint8
    , crc      :: Record CRC16
    , handle   :: Buffer n Uint8 -> forall eff. Ivory eff ()
    , transmit :: Uint8 -> forall eff. Ivory eff ()
    }


rxPreamble :: Preamble
rxPreamble = preambleMaster

txPreamble :: Preamble
txPreamble = preambleSlave


slave :: KnownNat n
      => String
      -> Mac
      -> Value Uint8
      -> Record Version
      -> (Buffer n Uint8 -> forall eff. Ivory eff ())
      -> (Uint8 -> forall eff. Ivory eff ())
      -> Slave n
slave name mac hwType version handle transmit = Slave
    { name     = name
    , mac      = mac
    , hwType   = hwType
    , version  = version
    , address  = value        (name <> "_address") broadcastAddress
    , state    = value        (name <> "_state") readyToReceive
    , phase    = value        (name <> "_phase") waitingAddress
    , index    = value        (name <> "_index") 0
    , size     = value        (name <> "_size") 0
    , buff     = buffer       (name <> "_message")
    , buffConf = buffer       (name <> "_confirm_tx")
    , buffPing = buffer       (name <> "_ping_tx")
    , buffDisc = buffer       (name <> "_disc_tx")
    , crc      = record       (name <> "_crc") initCRC16
    , handle   = handle
    , transmit = transmit
    }


instance Include (Slave n) where
    include (Slave {address, state, index, phase, size, buff, buffConf, buffPing, buffDisc, crc}) = do
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
        include crc


instance Initialize (Slave n) where
    initialize s = ($ s) <$> [initDisc, initConf, initPing]

initDisc :: Slave n -> Def('[] :-> ())
initDisc (Slave {name, mac, hwType, version, buffDisc}) =
    proc (name <> "_init_disc_tx") $ body $ do
        setItem buffDisc 0 $ discovery txPreamble
        arrayCopy (getBuffer buffDisc) (getBuffer mac) 1 (getSize mac)
        setItem buffDisc 8 =<< getValue hwType
        setItem buffDisc 9 =<< version |> major
        setItem buffDisc 10 =<< version |> minor
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
    let size = getSize buff :: Sint32
    let s_2 = toIx $ size - 2
    let s_1 = toIx $ size - 1
    crc <- local $ istruct initCRC16
    for s_2 $ updateCRC16 crc <=< getItem buff
    setItem buff s_2 =<< deref (crc ~> msb)
    setItem buff s_1 =<< deref (crc ~> lsb)


receiveData :: KnownNat n => Slave n -> Uint8 -> Ivory (AllocEffects s) ()
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



receiveDiscovery :: KnownNat n => Slave n -> Uint8 -> Ivory (AllocEffects s) ()
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
receiveDiscoveryLsbCRC s@(Slave {address, buff}) =
    receiveLsbCRC s $ do
        setValue address =<< getItem buff 0
        call_ $ initConf s
        call_ $ initPing s



receivePing :: KnownNat n => Slave n -> Uint8 -> Ivory (AllocEffects s) ()
receivePing = runReceive phase
    [ go waitingAddress $ receiveAddress waitingMsbCRC
    , go waitingMsbCRC    receiveMsbCRC
    , go waitingLsbCRC    receivePingLsbCRC
    ]

receivePingLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receivePingLsbCRC r@(Slave {address}) =
    receiveLsbCRC r $ setValue address broadcastAddress



receiveConfirm :: KnownNat n => Slave n -> Uint8 -> Ivory (AllocEffects s) ()
receiveConfirm = runReceive phase
    [ go waitingAddress $ receiveAddress waitingMsbCRC
    , go waitingMsbCRC    receiveMsbCRC
    , go waitingLsbCRC    receiveConfirmLsbCRC
    ]

receiveConfirmLsbCRC :: Slave n -> Uint8 -> Ivory eff ()
receiveConfirmLsbCRC r =
    receiveLsbCRC r undefined



receiveMessage :: KnownNat n => Slave n -> Uint8 -> Ivory (AllocEffects s) ()
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



transmitDiscovery :: Slave n -> Ivory (AllocEffects s) ()
transmitDiscovery (Slave {buffDisc, transmit}) =
    transmit' buffDisc transmit

transmitPing :: Slave n -> Ivory (AllowBreak(AllocEffects s)) ()
transmitPing (Slave {buffPing, transmit}) =
    transmit' buffPing transmit

transmitConfirm :: Slave n -> Ivory (AllowBreak(AllocEffects s)) ()
transmitConfirm (Slave {buffConf, transmit}) =
    transmit' buffConf transmit

transmitMessage :: KnownNat m
                => Slave n
                -> (Ix m -> forall eff. Ivory eff Uint8)
                -> Ix m
                -> Ivory (AllowBreak (AllocEffects s)) ()
transmitMessage (Slave{address, transmit}) get n = do
    crc16 <- local $ istruct initCRC16
    let t v = updateCRC16 crc16 v >> transmit v
    t $ message txPreamble
    t =<< getValue address
    t $ castDefault (fromIx n)
    for n $ t <=< get
    transmit =<< deref (crc16~>msb)
    transmit =<< deref (crc16~>lsb)

transmit' :: KnownNat n
          => Buffer n Uint8
          -> (Uint8 -> Ivory (AllowBreak eff) ())
          -> Ivory eff ()
transmit' buff transmit =
    arrayMap $ transmit <=< getItem buff
