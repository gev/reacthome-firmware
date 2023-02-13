{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

module Feature.Blink where

import           Control.Monad.Reader
import           Data.Function
import           Feature
import           Include
import           Initialize
import           Interface.GPIO
import           Interface.MCU
import           Interface.Timer
import           Ivory.Language
import           Util.Data.Class
import           Util.Data.Value

data Blink = forall o. OUT o => Blink
 { name  :: String
 , out   :: o
 , state :: Value IBool
 }


blink :: (MCU mcu, OUT o) => Int -> (mcu -> o) -> Reader mcu Feature
blink n out = do
    mcu <- ask
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
            v <- getValue state
            setValue state $ iNot v
            ifte_ v (set   out)
                    (reset out)
        ]
