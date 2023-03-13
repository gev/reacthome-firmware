{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Endpoint.Groups where

import           Core.Include
import           Data.Buffer
import           Data.Record
import           Data.Serialize
import           GHC.TypeNats
import           Ivory.Language



type GroupStruct = "group_struct"

[ivory|
    struct group_struct
    { enabled   :: IBool
    ; delay     :: Uint32
    ; synced    :: IBool
    }
|]



data Groups = Groups
    { runGroups :: RunRecords GroupStruct
    , payload   :: Buffer 7 Uint8
    }

groups :: String -> Int -> Groups
groups name n = Groups
    { runGroups = runRecords name $ replicate n go
    , payload   = buffer "group_message"
    } where go = [ enabled   .= ival false
                 , delay     .= ival 0
                 , synced    .= ival true
                 ]



message :: Groups -> Uint8 -> Ivory eff (Buffer 7 Uint8)
message (Groups runGroup payload) i = do
    let payload' = addrOf payload
    runGroup $ \r -> do
        let group = addrOf r ! toIx i
        pack   payload' 0 (2 :: Uint8)
        pack   payload' 1 $ i + 1
        pack   payload' 2 =<< deref (group ~> enabled)
        packLE payload' 3 =<< deref (group ~> delay)
    pure payload



instance KnownNat n => Include (Records n GroupStruct) where
    include r = do
        defStruct (Proxy :: Proxy GroupStruct)
        defMemArea r

instance Include Groups where
    include (Groups {..}) =
        runGroups include >> include payload
