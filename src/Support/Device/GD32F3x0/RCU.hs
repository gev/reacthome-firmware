{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

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

data RCU_PERIPH
  = RCU_GPIOA 
  deriving (Show, Enum, Bounded)

enablePeriphClock :: RCU_PERIPH -> Ivory eff ()
enablePeriphClock = call_ rcuPeriphClockEnable . extConst

inclRCU :: ModuleM ()
inclRCU = do
  traverse_ (inclSym . extPeriph) [minBound .. maxBound]
  incl rcuPeriphClockEnable

rcuPeriphClockEnable :: Def ('[Uint32] :-> ())
rcuPeriphClockEnable = extProc "rcu_periph_clock_enable"

extPeriph :: RCU_PERIPH -> Uint32
extPeriph = extConst

extConst :: (Show a, IvoryExpr e) => a -> e
extConst = (`extern` headerFile) . show

extProc :: ProcType t => Sym -> Def t
extProc = (`importProc` headerFile)

headerFile :: String
headerFile = "gd32f3x0_rcu.h"

