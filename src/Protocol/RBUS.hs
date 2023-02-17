{-# LANGUAGE DataKinds #-}
module Protocol.RBUS where

import           Data.Buffer
import           Data.Class
import           Data.Record
import           Ivory.Language
import           Ivory.Stdlib
import           Util.CRC16



data Preamble = Preamble
    { discovery :: Uint8
    , ping      :: Uint8
    , confirm   :: Uint8
    , message   :: Uint8
    }

preambleMaster = Preamble { discovery = 0xaa
                          , ping      = 0xcc
                          , confirm   = 0xaf
                          , message   = 0xa0
                          }

preambleSlave  = Preamble { discovery = 0x55
                          , ping      = 0x33
                          , confirm   = 0x5f
                          , message   = 0x50
                          }


broadcastAddress    = 0xff :: Uint8

readyToReceive      = 0x00 :: Uint8
receivingDiscovery  = 0x01 :: Uint8
receivingPing       = 0x02 :: Uint8
receivingConfirm    = 0x03 :: Uint8
receivingMessage    = 0x04 :: Uint8

waitingAddress      = 0x00 :: Uint8
waitingSize         = 0x01 :: Uint8
waitingData         = 0x02 :: Uint8
waitingMsbCRC       = 0x03 :: Uint8
waitingLsbCRC       = 0x04 :: Uint8

updateCRC :: Record CRC16 -> Uint8 -> Ivory eff ()
updateCRC = updateCRC16 . getRecord


go :: a -> b -> (a, b)
go = (,)


runReceive :: (Val v a, IvoryEq a)
           => (r -> v a)
           -> [(a, r -> Uint8 -> Ivory (ProcEffects s ()) ())]
           -> r
           -> Uint8
           -> Ivory (ProcEffects s ()) ()
runReceive f hs r v = do
    p <- getValue . f $ r
    let go (w, h) = w ==? p ==> h r v
    cond_ $ go <$> hs
