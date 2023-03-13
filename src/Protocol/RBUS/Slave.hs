
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
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
    include (Slave {..}) = do
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
initDisc (Slave {..}) =
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
initConf (Slave {..}) =
    proc (name <> "_init_conf_tx") $ body $ do
        let buffConf'    = addrOf buffConf
        store (buffConf' ! 0) $ confirm txPreamble
        store (buffConf' ! 1) =<< deref (addrOf address)
        calcCRC16 buffConf

initPing :: Slave n -> Def('[] :-> ())
initPing (Slave {..}) =
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



hasAddress :: Slave n -> Ivory eff IBool
hasAddress (Slave {..}) = do
    a <- deref $ addrOf address
    pure $ a /=? broadcastAddress
