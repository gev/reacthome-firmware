{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Support.Device.GD32F3x0.RCU
  ( RCU_PERIPH(..)
  , enablePeriphClock
  , inclRCU
  ) where

import           Data.Foldable
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Language.Proc
import           Ivory.Language.Syntax
import           Support.Ivory

(extConst, extProc) = include "gd32f3x0_rcu.h"

data RCU_PERIPH
  = RCU_GPIOA
  deriving (Show, Enum, Bounded)
instance ExtConst RCU_PERIPH Uint32


inclRCU :: ModuleM ()
inclRCU = do
  inclConst (extConst :: Ext RCU_PERIPH Uint32)
  incl rcuPeriphClockEnable


enablePeriphClock :: RCU_PERIPH -> Ivory eff ()
enablePeriphClock = call_ rcuPeriphClockEnable . extConst

rcuPeriphClockEnable :: Def ('[Uint32] :-> ())
rcuPeriphClockEnable = extProc "rcu_periph_clock_enable"
