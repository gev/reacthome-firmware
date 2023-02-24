{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Protocol.RBUS.Slave where

import           Core.Include
import           Core.Initialize
import           Core.Version    (Version, major, minor)
import           Data.Buffer
import           Data.Record
import           Data.Value
import           GHC.TypeNats
import           Interface.Mac   (Mac)
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RBUS
import           Util.CRC16



data Slave n = Slave
    { name          :: String
    , mac           :: Buffer  6 Uint8
    , model         :: Value     Uint8
    , version       :: Version
    , address       :: Value     Uint8
    , state         :: Value     Uint8
    , phase         :: Value     Uint8
    , index         :: Value     Uint8
    , size          :: Value     Uint8
    , buff          :: Buffer  n Uint8
    , buffConf      :: Buffer  4 Uint8
    , buffPing      :: Buffer  4 Uint8
    , buffDisc      :: Buffer 12 Uint8
    , tidRx         :: Value     Sint16
    , tidTx         :: Value     Uint8
    , crc           :: Record    CRC16
    , tmp           :: Value     Uint8
    , onMessage     :: Buffer n Uint8 -> Uint8 -> IBool -> forall s. Ivory (ProcEffects s ()) ()
    , onConfirm     :: forall eff. Ivory eff ()
    , onDiscovery   :: forall eff. Ivory eff ()
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
      -> (Buffer n Uint8 -> Uint8 -> IBool -> forall s. Ivory (ProcEffects s ()) ())
      -> (forall eff. Ivory eff ())
      -> (forall eff. Ivory eff ())
      -> Slave n
slave n mac model version onMessage onConfirm onDiscovery = Slave
    { name          = name
    , mac           = mac
    , model         = model
    , version       = version
    , address       = value      (name <> "_address")      broadcastAddress
    , state         = value      (name <> "_state")        readyToReceive
    , phase         = value      (name <> "_phase")        waitingAddress
    , index         = value      (name <> "_index")        0
    , size          = value      (name <> "_size")         0
    , buff          = buffer     (name <> "_message")
    , buffConf      = buffer     (name <> "_confirm_tx")
    , buffPing      = buffer     (name <> "_ping_tx")
    , buffDisc      = buffer     (name <> "_disc_tx")
    , tidRx         = value      (name <> "_tid_rx")     (-1)
    , tidTx         = value      (name <> "_tid_tx")       0
    , crc           = record     (name <> "_crc")          initCRC16
    , tmp           = value      (name <> "_tmp")          0
    , onMessage     = onMessage
    , onConfirm     = onConfirm
    , onDiscovery   = onDiscovery
    } where name = "protocol_" <> n


instance KnownNat n => Include (Slave n) where
    include (Slave {address, state, index, phase, size, buff, buffConf, buffPing, buffDisc, tidRx, tidTx, crc, tmp}) = do
        inclCRC16
        include tmp
        include address
        include state
        include phase
        include index
        include size
        include buff
        include buffConf
        include buffPing
        include buffDisc
        include tidRx
        include tidTx
        include crc
        include tmp


instance Initialize (Slave n) where
    initialize s = ($ s) <$> [initDisc, initConf, initPing]

initDisc :: Slave n -> Def('[] :-> ())
initDisc (Slave {name, mac, model, version, buffDisc}) =
    proc (name <> "_init_disc_tx") $ body $ do
        let mac'         = addrOf mac
        let model'       = addrOf model
        let version'     = addrOf version
        let buffDisc'    = addrOf buffDisc
        store (buffDisc' ! 0) $ discovery txPreamble
        arrayCopy buffDisc' mac' 1 $ arrayLen mac'
        store (buffDisc' ! 7) =<< deref model'
        store (buffDisc' ! 8) =<< deref (version' ~> major)
        store (buffDisc' ! 9) =<< deref (version' ~> minor)
        calcCRC16 buffDisc

initConf :: Slave n -> Def('[] :-> ())
initConf (Slave {name, address, buffConf}) =
    proc (name <> "_init_conf_tx") $ body $ do
        let buffConf'    = addrOf buffConf
        store (buffConf' ! 0) $ confirm txPreamble
        store (buffConf' ! 1) =<< deref (addrOf address)
        calcCRC16 buffConf

initPing :: Slave n -> Def('[] :-> ())
initPing (Slave {name, address, buffPing}) =
    proc (name <> "_init_ping_tx") $ body $ do
        let buffPing'    = addrOf buffPing
        store (buffPing' ! 0) $ ping txPreamble
        store (buffPing' ! 1) =<< deref (addrOf address)
        calcCRC16 buffPing

calcCRC16 :: KnownNat n => Buffer n Uint8 -> Ivory (ProcEffects s ()) ()
calcCRC16 buff = do
    let buff'    = addrOf buff
    let size     = arrayLen buff' :: Uint16
    let s_2      = toIx $ size - 2
    let s_1      = toIx $ size - 1
    crc <- local $ istruct initCRC16
    for s_2 $ \ix -> updateCRC16 crc =<< deref (buff' ! ix)
    store (buff' ! s_2)  =<< deref (crc ~> msb)
    store (buff' ! s_1) =<< deref (crc ~> lsb)


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



transmitMessage :: KnownNat l
                => Buffer l Uint8
                -> Slave n
                -> (Uint8 -> forall eff. Ivory eff ())
                -> Ivory (ProcEffects s ()) ()
transmitMessage payload (Slave{address, tidTx}) transmit = do
    let payload' = addrOf payload
    crc <- local $ istruct initCRC16
    let transmit' :: Uint8 -> Ivory eff ()
        transmit' v = updateCRC16 crc v >> transmit v
    transmit' $ message txPreamble
    transmit' =<< deref (addrOf address)
    id <- deref (addrOf tidTx)
    transmit' id
    store (addrOf tidTx) $ id + 1
    transmit' $ arrayLen payload'
    arrayMap $ \ix -> transmit' =<< deref (payload' ! ix)
    transmit =<< deref (crc ~> msb)
    transmit =<< deref (crc ~> lsb)



transmitDiscovery :: Slave n -> (Uint8 -> Ivory (AllowBreak eff) ()) -> Ivory eff ()
transmitDiscovery = transmit' . buffDisc

transmitPing :: Slave n -> (Uint8 -> Ivory (AllowBreak eff) ()) -> Ivory eff ()
transmitPing = transmit' . buffPing

transmitConfirm :: Slave n -> (Uint8 -> Ivory (AllowBreak eff) ()) -> Ivory eff ()
transmitConfirm = transmit' . buffConf

transmit' :: KnownNat n
          => Buffer n Uint8
          -> (Uint8 -> Ivory (AllowBreak eff) ())
          -> Ivory eff ()
transmit' buff transmit =
    arrayMap $ \ix -> transmit =<< deref (addrOf buff ! ix)



hasAddress :: Slave n -> Ivory eff IBool
hasAddress (Slave {address}) = do
    a <- deref $ addrOf address
    pure $ a /=? broadcastAddress
