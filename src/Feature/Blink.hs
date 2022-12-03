{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}

module Feature.Blink where

import           Feature
import           Interface      as I
import           Interface.GPIO as I
import           Ivory.Language


data Blink a = (I.OUT a) => Blink Int a

instance I.Interface (Blink b) where

  dependencies (Blink _ out) = I.dependencies out

  initialize (Blink n out) = I.initialize out



instance Task (Blink b) where

  step (Blink n out) =
    proc ("blink_" <> show n <> "_step") $ body $ do
      set out
      delay 10_000_000
      reset out
      delay 10_000_000


delay :: Ix 1_000_000_000 -> Ivory eff ()
delay n = n `times` pure
