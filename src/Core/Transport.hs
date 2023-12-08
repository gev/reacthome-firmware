{-# LANGUAGE RankNTypes #-}

module Core.Transport where

import           Core.Context
import           Core.Task
import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language


class LazyTransport x where
    lazyTransmit     :: x
                     -> Uint8
                     -> ((Uint8 -> forall eff. Ivory eff ())
                                -> forall eff. Ivory eff ())
                     -> Ivory (ProcEffects s t) ()

class Transport x where
    transmitBuffer   :: KnownNat n
                     => x
                     -> Buffer n Uint8
                     -> Ivory (ProcEffects s t) ()
