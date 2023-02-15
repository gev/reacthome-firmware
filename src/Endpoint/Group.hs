module Endpoint.Group where

import           Include
import           Initialize
import           Util.Data.Class
import           Util.Data.Value
import           Ivory.Language


data Group = Group
  {  number    :: Uint8
  ,  timestamp :: Uint32
  }
