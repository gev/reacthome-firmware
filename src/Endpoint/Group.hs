{-# LANGUAGE NamedFieldPuns #-}

module Endpoint.Group where

import           Core.Include
import           Core.Initialize
import           Data.Class
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
  } where name = "group_" <> show n

-- group_enable n = undefined

-- group_disable n = undefined

instance Include Group where
  include (Group {enabled, timestamp, delay}) = include enabled >> include timestamp >> include delay
