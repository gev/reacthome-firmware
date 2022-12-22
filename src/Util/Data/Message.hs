{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Util.Data.Message where

import           Ivory.Language


data Message t l = Message
 { msgData :: forall r. Ref r (Array l (Stored t))
 , msgSize :: Ix l
 }
