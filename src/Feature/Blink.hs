{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

module Feature.Blink where

import           Data.Function
import           Device.GD32F3x0.SystemClock
import           Feature
import           Include
import           Initialize
import           Interface.GPIO
import           Interface.Timer
import           Ivory.Language
import           Util.Data.Class
import           Util.Data.Value


data Blink = forall a. (OUT a) => Blink
 { name  :: String
 , out   :: a
 , state :: Value IBool
 }


blink n out = Feature $ Blink
  { name  = name
  , out   = out
  , state = value (name <> "_state") false
  } where name = "blink_" <> show n


instance Include Blink where
  include (Blink {out, state}) = include state >> include out


instance Initialize Blink where
  initialize (Blink {out}) = initialize out


instance Task Blink where
  tasks (Blink name out state) = [
    delay 1_000 name $ do
      v <- getValue state
      setValue state $ iNot v
      ifte_ v ( set out )
              ( reset out )
    ]
