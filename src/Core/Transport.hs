module Core.Transport where

import           Core.Task
import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language


class Task t => Transport t where
    transmit :: KnownNat l => t -> Buffer l Uint8 -> Ivory (ProcEffects s ()) ()
