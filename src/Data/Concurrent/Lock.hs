module Data.Concurrent.Lock where

import           Ivory.Language

lock :: Ivory eff ()
lock = pure ()

unlock :: Ivory eff ()
unlock = pure ()

atomically :: Ivory eff () -> Ivory eff ()
atomically run = lock >> run >> unlock
