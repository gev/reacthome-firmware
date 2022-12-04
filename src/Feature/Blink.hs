{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}

module Feature.Blink where

import           Feature
import           Interface      as I
import           Interface.GPIO as I
import           Ivory.Language


data Blink a = (I.OUT a) => Blink Int a

state :: Int -> MemArea ('Stored IBool)
state n = area ("blink_" <> show n <> "_state") (Just (ival false))

instance I.Interface (Blink b) where

  dependencies (Blink n out) =
    let s = state n
    in defMemArea s : I.dependencies out

  initialize (Blink _ out) = I.initialize out


instance Task (Blink b) where

  task (Blink n out) =
    Step (Just 1_000) $ proc ("blink_" <> show n <> "_step") $ body $ do
      let s = addrOf $ state n
      v <- deref s
      ifte_ v (set out) (reset out)
      store s $ iNot v
