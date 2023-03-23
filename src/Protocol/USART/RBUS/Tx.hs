{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Protocol.USART.RBUS.Tx where

import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language
import           Protocol.USART.RBUS
import           Util.CRC16


transmitMessage :: KnownNat l
                => Buffer l Uint8
                -> RBUS n
                -> (Uint8 -> forall eff. Ivory eff ())
                -> Ivory (ProcEffects s ()) ()
transmitMessage payload (RBUS {..}) transmit = do
    crc <- local $ istruct initCRC16
    let transmit' :: Uint8 -> Ivory eff ()
        transmit' v = updateCRC16 crc v >> transmit v
    transmit' $ message preamble
    id <- deref tidTx
    transmit' id
    store tidTx $ id + 1
    transmit' $ arrayLen payload
    arrayMap $ \ix -> transmit' =<< deref (payload ! ix)
    transmit =<< deref (crc ~> msb)
    transmit =<< deref (crc ~> lsb)



transmit' :: KnownNat n
          => Buffer n Uint8
          -> (Uint8 -> Ivory (AllowBreak eff) ())
          -> Ivory eff ()
transmit' buff transmit =
    arrayMap $ \ix -> transmit =<< deref (buff ! ix)
