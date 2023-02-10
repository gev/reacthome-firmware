{-# LANGUAGE GADTs #-}

module Device.GD32F3x0 where

import           Interface.SystemClock

data GD32F3x0 where
    GD32F3x0 :: { systemClock :: SystemClock
                } -> GD32F3x0
