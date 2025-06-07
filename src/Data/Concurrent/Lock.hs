module Data.Concurrent.Lock (atomically) where

import           Ivory.Language
import           Support.CMSIS.CoreCMFunc (disableIRQ, enableIRQ)

lock :: Ivory eff ()
lock = disableIRQ

unlock :: Ivory eff ()
unlock = enableIRQ

atomically :: Ivory eff () -> Ivory eff ()
atomically run = lock >> run >> unlock
