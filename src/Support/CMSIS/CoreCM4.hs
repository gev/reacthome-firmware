{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Support.CMSIS.CoreCM4
  ( nop
  , inclCoreCM4
  ) where

import           Control.Monad
import           Ivory.Language
import           Ivory.Language.Module
import           Support.Ivory

(_, fun) = include "core_cm4.h"


inclCoreCM4 :: ModuleM ()
inclCoreCM4 = do
  incl __NOP


nop :: Int -> Ivory eff ()
nop n = replicateM_ n (call_ __NOP)

__NOP :: Def ('[] :-> ())
__NOP = fun "__NOP"
