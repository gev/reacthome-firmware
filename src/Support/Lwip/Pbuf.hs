{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Support.Lwip.Pbuf (
    PBUF_STRUCT,
    PBUF,
    len,
    tot_len,
    PBUF_LAYER,
    pbuf_transport,
    pbuf_ip,
    pbuf_link,
    pbuf_raw_tx,
    pbuf_raw,
    PBUF_TYPE,
    pbuf_ram,
    pbuf_rom,
    pbuf_ref,
    pbuf_pool,
    freePbuf,
    allocPbuf,
    allocPbufReference,
    getPbufAt,
    memCmpPbuf,
    memFindPbuf,
    putPbufAt,
    takePbuf,
    takePbufAt,
    tryGetPbufAt,
    chainPbuf,
    inclPbuf,
) where

import Ivory.Language
import Ivory.Language.Proc (ProcType)
import Ivory.Language.Syntax (Sym)
import Ivory.Support
import Support.Lwip.Err

headerFile :: HeaderFile
headerFile = "lwip/pbuf.h"

ext :: (IvoryExpr e) => Sym -> e
ext = extFrom headerFile

fun :: (ProcType f) => Sym -> Def f
fun = funFrom headerFile

newtype PBUF_LAYER = PBUF_LAYER Sint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

pbuf_transport = PBUF_LAYER $ ext "PBUF_TRANSPORT"
pbuf_ip = PBUF_LAYER $ ext "PBUF_IP"
pbuf_link = PBUF_LAYER $ ext "PBUF_LINK"
pbuf_raw_tx = PBUF_LAYER $ ext "PBUF_RAW_TX"
pbuf_raw = PBUF_LAYER $ ext "PBUF_RAW"

newtype PBUF_TYPE = PBUF_TYPE Sint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

pbuf_ram = PBUF_TYPE $ ext "PBUF_RAM"
pbuf_rom = PBUF_TYPE $ ext "PBUF_ROM"
pbuf_ref = PBUF_TYPE $ ext "PBUF_REF"
pbuf_pool = PBUF_TYPE $ ext "PBUF_POOL"

type PBUF_STRUCT = "pbuf"
type PBUF s = Ref s (Struct PBUF_STRUCT)

[ivory|
    struct pbuf
    { len :: Stored Uint16
    ; tot_len :: Stored Uint16
    }
|]

freePbuf :: PBUF s -> Ivory eff ErrT
freePbuf = call pbuf_free

pbuf_free :: Def ('[PBUF s] :-> ErrT)
pbuf_free = fun "pbuf_free"

allocPbuf :: PBUF_LAYER -> Uint16 -> PBUF_TYPE -> Ivory eff (PBUF s)
allocPbuf = call pbuf_alloc

pbuf_alloc :: Def ('[PBUF_LAYER, Uint16, PBUF_TYPE] :-> PBUF s)
pbuf_alloc = fun "pbuf_alloc"

allocPbufReference :: Ref s1 (CArray (Stored Uint8)) -> Uint16 -> PBUF_TYPE -> Ivory eff (PBUF s2)
allocPbufReference = call pbuf_alloc_reference

pbuf_alloc_reference :: Def ('[Ref s1 (CArray (Stored Uint8)), Uint16, PBUF_TYPE] :-> PBUF s2)
pbuf_alloc_reference = fun "pbuf_alloc_reference"

getPbufAt :: PBUF s -> Uint16 -> Ivory eff Uint8
getPbufAt = call pbuf_get_at

pbuf_get_at :: Def ('[PBUF s, Uint16] :-> Uint8)
pbuf_get_at = fun "pbuf_get_at"

memCmpPbuf :: PBUF s1 -> Uint16 -> Ref s2 (CArray (Stored Uint8)) -> Ivory eff Uint16
memCmpPbuf = call pbuf_memcmp

pbuf_memcmp :: Def ('[PBUF s1, Uint16, Ref s2 (CArray (Stored Uint8))] :-> Uint16)
pbuf_memcmp = fun "pbuf_memcmp"

memFindPbuf :: PBUF s1 -> Ref s2 (CArray (Stored Uint8)) -> Uint16 -> Uint16 -> Ivory eff Uint16
memFindPbuf = call pbuf_memfind

pbuf_memfind :: Def ('[PBUF s1, Ref s2 (CArray (Stored Uint8)), Uint16, Uint16] :-> Uint16)
pbuf_memfind = fun "pbuf_memfind"

putPbufAt :: PBUF s -> Uint16 -> Uint8 -> Ivory eff ()
putPbufAt = call_ pbuf_put_at

pbuf_put_at :: Def ('[PBUF s, Uint16, Uint8] :-> ())
pbuf_put_at = fun "pbuf_put_at"

takePbuf :: PBUF s1 -> Ref s2 (CArray (Stored Uint8)) -> Uint16 -> Ivory eff ErrT
takePbuf = call pbuf_take

pbuf_take :: Def ('[PBUF s1, Ref s2 (CArray (Stored Uint8)), Uint16] :-> ErrT)
pbuf_take = fun "pbuf_take"

takePbufAt :: PBUF s1 -> Ref s2 (CArray (Stored Uint8)) -> Uint16 -> Uint16 -> Ivory eff ErrT
takePbufAt = call pbuf_take_at

pbuf_take_at :: Def ('[PBUF s1, Ref s2 (CArray (Stored Uint8)), Uint16, Uint16] :-> ErrT)
pbuf_take_at = fun "pbuf_take_at"

tryGetPbufAt :: PBUF s -> Uint16 -> Ivory eff Sint32
tryGetPbufAt = call pbuf_try_get_at

pbuf_try_get_at :: Def ('[PBUF s, Uint16] :-> Sint32)
pbuf_try_get_at = fun "pbuf_try_get_at"

chainPbuf :: PBUF s1 -> PBUF s2 -> Ivory eff ()
chainPbuf = call_ pbuf_chain

pbuf_chain :: Def ('[PBUF s1, PBUF s2] :-> ())
pbuf_chain = fun "pbuf_chain"

inclPbuf :: ModuleDef
inclPbuf = do
    inclSym pbuf_transport
    inclSym pbuf_ip
    inclSym pbuf_link
    inclSym pbuf_raw_tx
    inclSym pbuf_raw
    inclSym pbuf_ram
    inclSym pbuf_rom
    inclSym pbuf_ref
    inclSym pbuf_pool

    incl pbuf_free
    incl pbuf_alloc
    incl pbuf_alloc_reference
    incl pbuf_get_at
    incl pbuf_memcmp
    incl pbuf_memfind
    incl pbuf_put_at
    incl pbuf_take
    incl pbuf_take_at
    incl pbuf_try_get_at
    incl pbuf_chain
