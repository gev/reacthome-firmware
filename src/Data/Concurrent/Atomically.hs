module Data.Concurrent.Atomically where

import Ivory.Language
import Support.CMSIS.CoreCMFunc (disableIRQ, enableIRQ)

atomically :: Ivory eff () -> Ivory eff ()
atomically run = disableIRQ >> run >> enableIRQ
