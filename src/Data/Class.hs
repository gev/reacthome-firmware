{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Class where

import           GHC.Base
import           GHC.TypeNats
import           Include
import           Ivory.Language
import           Ivory.Language.Pointer


class Include (v t) => Val v t where
    getValue :: v t -> Ivory eff t
    setValue :: v t -> t -> Ivory eff ()


class (Include (b n t), KnownNat n,  IvoryStore t, IvoryType t) => Buff b n t where
    getBuffer :: b n t -> Ref Global (Array n (Stored t))
    setItem   :: b n t -> Ix n -> t -> Ivory eff ()
    getItem   :: b n t -> Ix n -> Ivory eff t
    getSize   :: Num len => b n t -> len

    setItem b ix = store (getBuffer b ! ix)
    getItem b ix = deref (getBuffer b ! ix)
    getSize      = arrayLen . getBuffer


class (IvoryStruct s, Include (r s)) => Rec r (s :: Symbol) where
    getRecord :: r s -> Ref Global (Struct s)
    (<|)      :: IvoryStore t => r s -> Label s (Stored t) -> t -> Ivory eff ()
    (|>)      :: IvoryStore t => r s -> Label s (Stored t) -> Ivory eff t

    r <| f = store $ getRecord r ~> f
    r |> f = deref $ getRecord r ~> f
