{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Data.Record where

import           Include
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Language.Pointer
import           Data.Class


data Record t = Record
    { defRecord  :: ModuleM ()
    , addrRecord :: Ref Global (Struct t)
    }


record :: IvoryStruct t => String -> [InitStruct t] -> Record t
record id r = Record { defRecord  = defMemArea a
                     , addrRecord = addrOf a
                     } where a    = area id $ Just (istruct r)


instance Include (Record t) where
    include = defRecord


instance IvoryStruct t => Rec Record t where
    getRecord = addrRecord
