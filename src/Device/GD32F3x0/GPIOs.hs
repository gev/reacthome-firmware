{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

module Device.GD32F3x0.GPIOs where

import           Core.Context
import           Data.Record
import qualified Device.GD32F3x0.GPIO          as D
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Support.Device.GD32F3x0



type GPIOStruct = "gpio_struct"

[ivory|
    struct gpio_struct
    { gpio :: Uint32
    ; pin  :: Uint32
    }
|]



fromPort :: D.Port -> [InitStruct GPIOStruct]
fromPort p = [ gpio .= ival (def $ D.gpio p)
             , pin  .= ival (def $ D.pin  p)
             ]



runGPIO :: (Uint32 -> Uint32 -> Ivory eff a)
        -> RunRecords GPIOStruct
        -> (forall n. KnownNat n => Ix n) -> Ivory eff a
runGPIO f run ix = run $ \o -> do
    let o' = addrOf o
    gpio' <- deref (o' ! ix ~> gpio)
    pin'  <- deref (o' ! ix ~> pin )
    f gpio' pin'
