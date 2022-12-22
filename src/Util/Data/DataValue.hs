{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Util.Data.DataValue where

import           GHC.TypeNats
import           Include
import           Ivory.Language
import           Util.Data.Value


data DataValue t =
  (IvoryType t, IvoryStore t) =>
    DataValue (MemArea (Stored t))


maybeValue :: (IvoryType t, IvoryStore t, IvoryZeroVal t, IvoryInit t)
       => String -> Maybe t -> DataValue t
maybeValue id v = DataValue $ area id $ ival <$> v

emptyValue :: (IvoryType t, IvoryStore t, IvoryZeroVal t, IvoryInit t)
       => String -> DataValue t
emptyValue id = maybeValue id Nothing

justValue :: (IvoryType t, IvoryStore t, IvoryZeroVal t, IvoryInit t)
       => String -> t -> DataValue t
justValue id v = maybeValue id $ Just v


instance Include (DataValue t) where
  include (DataValue v) = defMemArea v


-- instance Value DataValue t where
storeValue (DataValue v) = store $ addrOf v
loadValue (DataValue v)  = deref $ addrOf v
