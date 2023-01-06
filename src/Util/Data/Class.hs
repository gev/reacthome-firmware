{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Util.Data.Class where

import           GHC.TypeLits   (KnownNat)
import           Include
import           Ivory.Language


class Include (v t) => Val v t where
    setValue :: v t -> t -> Ivory eff ()
    getValue :: v t -> Ivory eff t


class (Include (b n t), KnownNat n, IvoryType t) => Buff b n t where
    getBuffer :: b n t -> Ref Global (Array n (Stored t))
    setItem   :: b n t -> Ix n -> t -> Ivory eff ()
    getItem   :: b n t -> Ix n -> Ivory eff t

    size      :: Num len => b n t -> len
    size      = arrayLen . getBuffer
