{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Util.Data.Buffer where

import           Ivory.Language


class Buffer b n t  where
  storeItem     :: b n t -> Ix n -> t -> Ivory eff ()
  loadItem      :: b n t -> Ix n -> Ivory eff t
  processBuffer :: b n t
                -> (Ref 'Global ('Array n ('Stored t)) -> Ivory eff ())
                -> Ivory eff ()
