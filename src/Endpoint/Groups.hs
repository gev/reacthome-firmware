{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

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




getEnabled :: Groups -> (forall n. KnownNat n => Ix n) -> Ivory eff IBool
getEnabled = get enabled

getDelay :: Groups -> (forall n. KnownNat n => Ix n) -> Ivory eff Uint32
getDelay = get delay



setEnabled :: IBool -> Groups -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
setEnabled = set enabled

setDelay :: Uint32 -> Groups -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
setDelay = set delay



disable :: Groups -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
disable = setEnabled false

enable :: Groups -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
enable = setEnabled true

setState :: IBool -> Uint32 -> Groups -> (forall n. KnownNat n => Ix n) -> Ivory eff ()
setState e d gs i = do
    setEnabled e gs i
    setDelay d gs i



get :: IvoryStore t
    => Label GroupStruct (Stored t)
    -> Groups
    -> (forall n. KnownNat n => Ix n)
    -> Ivory eff t
get f gs i = runGroups gs $ \r -> deref (addrOf r ! i ~> f)

set :: IvoryStore t
    => Label GroupStruct (Stored t)
    -> t
    -> Groups
    -> (forall n. KnownNat n => Ix n)
    -> Ivory eff ()
set f v gs i = runGroups gs $ \r -> store (addrOf r ! i ~> f) v



instance KnownNat n => Include (Records n GroupStruct) where
    include r = do
        defStruct (Proxy :: Proxy GroupStruct)
        defMemArea r

instance Include Groups where
    include (Groups {runGroups, payload}) =
        runGroups include >> include payload
