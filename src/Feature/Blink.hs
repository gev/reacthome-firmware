{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
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


data Blink = forall a. (OUT a) => Blink Int a

state :: Int -> MemArea (Stored IBool)
state n = area ("blink_" <> show n <> "_state") (Just (ival false))


instance Include Blink where
  include (Blink n out) = defMemArea (state n) >> include out

instance Initialize Blink where
  initialize (Blink _ out) = initialize out


instance Task Blink where
  tasks (Blink n out) = [
    Step (Just 1_000) $ proc ("blink_" <> show n <> "_step") $ body $ do
      let s = addrOf $ state n
      v <- deref s
      store s $ iNot v
      ifte_ v ( set out )
              ( reset out )
    ]
