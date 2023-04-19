{-# LANGUAGE RankNTypes #-}

module Core.Transport where

import           Core.Context
import           Core.Task
import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language


class Transport t where

    runTransmit      :: t
                     -> ((Uint8 -> forall eff. Ivory eff ()) -> forall eff. Ivory eff ())
                     -> Ivory (ProcEffects s ()) ()

    transmitFragment :: KnownNat n
                     => t
                     -> Buffer n Uint8
                     -> Ix n
                     -> Ivory (ProcEffects s ()) ()

    transmitBuffer   :: KnownNat n
                     => t
                     -> Buffer n Uint8
                     -> Ivory (ProcEffects s ()) ()

    transmitBuffer t b = transmitFragment t b $ arrayLen b
