{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Util.Data.Class where

import           Include
import           Ivory.Language


class Include (v t) => Val v t where
  getValue :: v t -> Ivory eff t
  setValue :: v t -> t -> Ivory eff ()


class Include (b n t) => Buff b n t where
  setItem :: b n t -> Ix n -> t -> Ivory eff ()
  getItem :: b n t -> Ix n -> Ivory eff t
  process :: b n t -> (Ref Global (Array n (Stored t)) -> Ivory eff ())
                   -> Ivory eff ()
