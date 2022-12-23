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
 { n     :: Int
 , out   :: a
 , state :: Value IBool
 }


blink n out = Feature $ Blink
  { n     = n
  , out   = out
  , state = value (name n "state") false
  }

instance Include Blink where
  include (Blink n out state) = include state >> include out

instance Initialize Blink where
  initialize (Blink {out}) = initialize out


instance Task Blink where
  tasks (Blink n out state) = [
    Step (Just 1_000) $ proc (name n "step") $ body $ do
      v <- getValue state
      setValue state $ iNot v
      ifte_ v ( set out )
              ( reset out )
    ]


name :: Int -> String -> String
name n id = "blink_" <> show n <> "_" <> id
