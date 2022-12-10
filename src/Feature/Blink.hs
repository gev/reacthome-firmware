{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}

module Feature.Blink where

import           Data.Function
import           Device.GD32F3x0.SystemTimer
import           Feature
import           Interface                   as I
import           Interface.GPIO              as I
import           Interface.Timer
import           Ivory.Language


data Blink = forall a. (I.OUT a) => Blink Int a

state :: Int -> MemArea (Stored IBool)
state n = area ("blink_" <> show n <> "_state") (Just (ival false))


instance I.Interface Blink where
  dependencies (Blink n out) = defMemArea (state n) : I.dependencies out
  initialize (Blink _ out) = I.initialize out


instance Task Blink where
  task (Blink n out) =
    Step (Just 1_000) $ proc ("blink_" <> show n <> "_step") $ body $ do
      let s = addrOf $ state n
      v <- deref s
      store s $ iNot v
      ifte_ v ( do
                  set out
                  delay systemTimer 8_400_000
                  reset out
              )
              ( do
                  reset out
                  delay systemTimer 8_400_000
                  set out
              )
