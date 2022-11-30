{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeOperators      #-}

module Feature.Blink where

import           Feature
import           Interface             as I
import           Interface.GPIO        as I
import           Ivory.Language
import           Ivory.Language.Module


data Blink a = (I.OUT a) => Blink Int a

instance Prepare (Blink b) where
  prepare (Blink n out) =
     Pack (I.dependecies out)
          (prepare' n out)
          (step' n out)

prepare' :: OUT b => Int -> b -> Def ('[] :-> ())
prepare' n out =
  proc ("blink_" <> show n <> "_init") $ body $
    I.initialize out

step' :: OUT b => Int -> b -> Def ('[] :-> ())
step' n out =
  proc ("blink_" <> show n <> "_step") $ body $ do
    set out
    delay 10_000_000
    reset out
    delay 10_000_000

delay :: Ix 1_000_000_000 -> Ivory eff ()
delay n = n `times` pure
