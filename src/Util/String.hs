module Util.String (
    inclString,
    memCmp,
    memCpy,
) where

import GHC.TypeNats
import Ivory.Language
import Ivory.Language.Array
import Ivory.Language.Proxy

inclString :: ModuleDef
inclString = do
    incl memcmp
    incl memcpy

memcmp ::
    Def
        ( '[ Ref s1 (CArray (Stored Uint8))
           , Ref s2 (CArray (Stored Uint8))
           , IxRep
           ]
            ':-> IxRep
        )
memcmp = importProc "memcmp" "string.h"

memcpy ::
    Def
        ( '[ Ref s1 (CArray (Stored Uint8))
           , Ref s2 (CArray (Stored Uint8))
           , IxRep
           ]
            ':-> ()
        )
memcpy = importProc "memcpy" "string.h"

memCmp ::
    forall n s1 s2 eff.
    (KnownNat n) =>
    Ref s1 (Array n (Stored Uint8)) ->
    Ref s2 (Array n (Stored Uint8)) ->
    Ivory eff IxRep
memCmp a1 a2 =
    call
        memcmp
        (toCArray a1)
        (toCArray a2)
        (fromIntegral $ fromTypeNat (aNat :: NatType n))

memCpy ::
    forall n s1 s2 eff.
    (KnownNat n) =>
    Ref s1 (Array n (Stored Uint8)) ->
    Ref s2 (Array n (Stored Uint8)) ->
    Ivory eff ()
memCpy a1 a2 =
    call_
        memcpy
        (toCArray a1)
        (toCArray a2)
        (fromIntegral $ fromTypeNat (aNat :: NatType n))
