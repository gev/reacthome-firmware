module Endpoint.Group where

import           Core.Include
import           Core.Initialize
import           Data.Class
import           Data.Value
import           Ivory.Language


data Group = Group
  {  number    :: Uint8
  ,  timestamp :: Uint32
  }
