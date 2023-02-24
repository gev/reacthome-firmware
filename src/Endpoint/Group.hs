{-# LANGUAGE NamedFieldPuns #-}

module Endpoint.Group where

import           Core.Include
import           Core.Initialize
import           Data.Value
import           Ivory.Language


data Group = Group
  { enabled   :: Value IBool
  , timestamp :: Value Uint32
  , delay     :: Value Uint32
  }


group n = Group
  { enabled   = value (name <> "enabled")   false
  , timestamp = value (name <> "timestamp") 0
  , delay     = value (name <> "delay")     0
  }
  where name = "group_" <> show n

enableGroup :: Group -> Ivory eff ()
enableGroup (Group {enabled}) = store (addrOf enabled) true

disableGroup :: Group -> Ivory eff ()
disableGroup (Group {enabled}) =  store (addrOf enabled) false

setTimestamp :: Group -> Uint32 -> Ivory eff ()
setTimestamp (Group {timestamp}) = store $ addrOf timestamp

getTimestamp :: Group -> Ivory eff Uint32
getTimestamp (Group {timestamp}) = deref $ addrOf timestamp

setDelay :: Group -> Uint32 -> Ivory eff ()
setDelay (Group {delay}) = store $ addrOf delay


instance Include Group where
  include (Group {enabled, timestamp, delay}) =
    include enabled >> include timestamp >> include delay
