{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Util.Data.Value where

import           Ivory.Language


class Value v t where
  storeValue   :: v t -> t -> Ivory eff ()
  loadValue    :: v t -> Ivory eff t
