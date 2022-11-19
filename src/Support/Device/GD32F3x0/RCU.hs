{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.RCU
  ( RCU_PERIPH(..)
  , enablePeriphClock
  , inclRCU
  ) where
import           Ivory.Language
import           Ivory.Language.Module
import           Support.Ivory

(cast, fun) = include "gd32f3x0_rcu.h"

data RCU_PERIPH
  = RCU_GPIOA
  | RCU_TIMER2
  deriving (Show, Enum, Bounded)
instance ExtDef RCU_PERIPH Uint32


inclRCU :: ModuleM ()
inclRCU = do
  inclDef (cast :: Cast RCU_PERIPH Uint32)
  incl rcu_periph_clock_enable


enablePeriphClock :: RCU_PERIPH -> Ivory eff ()
enablePeriphClock = call_ rcu_periph_clock_enable . cast

rcu_periph_clock_enable :: Def ('[Uint32] :-> ())
rcu_periph_clock_enable = fun "rcu_periph_clock_enable"
