{-# LANGUAGE RankNTypes #-}

module Core.Transport where

import           Core.Context
import           Core.Task
import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language


class LazyTransport t where
    lazyTransmit     :: t
                     -> ((Uint8 -> forall eff. Ivory eff ())
                                -> forall eff. Ivory eff ())
                     -> Ivory (ProcEffects s ()) ()

class Transport t where
    transmitBuffer   :: KnownNat n
                     => t
                     -> Buffer n Uint8
                     -> Ivory (ProcEffects s ()) ()
