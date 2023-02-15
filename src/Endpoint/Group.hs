module Endpoint.Group where

import           Data.Class
import           Data.Value
import           Include
import           Initialize
import           Ivory.Language


data Group = Group
  {  number    :: Uint8
  ,  timestamp :: Uint32
  }
