{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeOperators      #-}

module Feature.Blink where


import           Device.GPIO           as GPIO
import           Feature
import           Ivory.Language
import           Ivory.Language.Module


data Blink a = Blink Int a

instance OUT b => Prepare (Blink b) where
  prepare (Blink n out) =
     Pack (GPIO.dependecies out)
          (prepare' n out)
          (step' n out)

prepare' :: OUT b => Int -> b -> Def ('[] ':-> ())
prepare' n out =
  proc ("blink_" <> show n <> "_init") $ body $
    GPIO.initialize out

step' :: OUT b => Int -> b -> Def ('[] ':-> ())
step' n out =
  proc ("blink_" <> show n <> "_step") $ body $ do
    set out
    delay 10_000_000
    reset out
    delay 10_000_000

delay :: Ix 1_000_000_000 -> Ivory eff ()
delay n = n `times` pure
