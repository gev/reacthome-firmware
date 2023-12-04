{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Transport.UDP.RBUS.Tx where
import           Control.Monad           (void)
import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib
import           Support.Lwip.IP_addr
import           Support.Lwip.Pbuf
import           Support.Lwip.Udp
import           Transport.UDP.RBUS.Data


transmit :: KnownNat n => RBUS -> Buffer n Uint8 -> Ivory (ProcEffects s t) ()
transmit rbus@RBUS{..} = transmit' serverIP rbus


broadcast :: KnownNat n => RBUS -> Buffer n Uint8 -> Ivory (ProcEffects s t) ()
broadcast rbus@RBUS{..} = transmit' broadcastIP rbus



transmit' :: KnownNat n => IP_ADDR_4 s1 -> RBUS -> Buffer n Uint8 -> Ivory (ProcEffects s2 t) ()
transmit' ip RBUS{..} buff = do
    upcb' <- deref upcb
    err <- connectUdp upcb' ip =<< deref serverPort
    when (err ==? 0) $ do
        arrayMap $ \ix -> store (txBuff ! toIx (6 + fromIx ix)) =<< deref (buff ! ix)
        pbuff <- allocPbufReference (toCArray txBuff) (6 + arrayLen buff) pbuf_ref
        sendUdp upcb' pbuff
        disconnectUdp upcb'
        void $ freePbuf pbuff



lazyTransmit' :: RBUS -> ((Uint8 -> forall eff. Ivory eff ())
                                 -> forall eff. Ivory eff ())
                      -> Ivory (ProcEffects s t) ()
lazyTransmit' RBUS{..} transmit = do
    upcb' <- deref upcb
    err <- connectUdp upcb' serverIP =<< deref serverPort
    when (err ==? 0) $ do
        len   <- run transmit txBuff
        pbuff <- allocPbufReference (toCArray txBuff) len pbuf_ref
        sendUdp upcb' pbuff
        disconnectUdp upcb'
        void $ freePbuf pbuff


run :: KnownNat n
    => ((Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s t) ())
    -> Buffer n Uint8
    -> Ivory (ProcEffects s t) Uint16
run transmit buff = do
    len <- local $ ival 6
    let go :: Uint8 -> Ivory eff ()
        go v = do
            i <- deref len
            let ix = toIx i
            store (buff ! ix) v
            store len $ i + 1
    transmit go
    deref len
