module Core.Transport where

import           Core.Context
import           Core.Task
import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language


class Transport t where
    transmit  :: KnownNat n => t -> Buffer n Uint8 -> Ix n -> Ivory (ProcEffects s ()) ()
    transmit_ :: KnownNat n => t -> Buffer n Uint8 -> Ivory (ProcEffects s ()) ()
    transmit_ t b = transmit t b $ arrayLen b
