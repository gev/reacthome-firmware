{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

module Feature.Blink where

import           Control.Monad.Reader
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Include
import           Core.Initialize
import           Core.Task
import           Data.Function
import           Data.Value
import           Interface.GPIO
import           Interface.MCU
import           Interface.Timer
import           Ivory.Language

data Blink = forall o. OUT o => Blink
 { name  :: String
 , out   :: o
 , state :: Value IBool
 }


blink :: (MCU mcu, OUT o) => Int -> (mcu -> o) -> Reader (Domain mcu t) Feature
blink n out = do
    mcu <- asks mcu
    pure $ Feature $ Blink { name  = name
                           , out   = out mcu
                           , state = value (name <> "_state") false
                           }
    where name = "blink_" <> show n



instance Include Blink where
    include (Blink {out, state}) = include state >> include out


instance Initialize Blink where
    initialize (Blink {out}) = initialize out


instance Task Blink where
    tasks (Blink name out state) = [
        delay 1_000 name $ do
            v <- deref $ addrOf state
            store (addrOf state) $ iNot v
            ifte_ v (set   out)
                    (reset out)
        ]


instance Controller Blink
