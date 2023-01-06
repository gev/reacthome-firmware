{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}

module Util.Data.Message where

import           Ivory.Language


type Message = Struct "message_struct"


[ivory|
    struct message_struct
        { msgData :: Uint16
        ; msgSize :: Uint16
        }
|]
