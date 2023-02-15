{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Value where

import           Core.Include
import           Data.Class
import           Ivory.Language
import           Ivory.Language.Module


data Value t = Value
    { defValue  :: ModuleM ()
    , addrValue :: Ref Global (Stored t)
    }


value :: (IvoryZeroVal t, IvoryInit t) => String -> t -> Value t
value id v = Value { defValue  = defMemArea a
                   , addrValue = addrOf a
                   } where a   = area id $ Just (ival v)


instance Include (Value t) where
    include = defValue


instance (IvoryStore t) => Val Value t where
    getValue = deref . addrValue
    setValue = store . addrValue
