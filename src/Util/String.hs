{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}


module Util.String
    ( inclString
    , memCmp
    , memCpy
    ) where

import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Array
import           Ivory.Language.CArray
import           Ivory.Language.Proxy



type Len = IxRep



inclString :: ModuleDef
inclString = do
    incl memcmp
    incl memcpy



memcmp :: Def ('[ Ref s1 (CArray (Stored Uint8))
                , Ref s2 (CArray (Stored Uint8))
                , IxRep] ':-> Len)
memcmp = importProc "memcmp" "string.h"


memcpy :: Def ('[ Ref s1 (CArray (Stored Uint8))
                , Ref s2 (CArray (Stored Uint8))
                , Len] ':-> Len)
memcpy = importProc "memcpy" "string.h"




memCmp :: forall n s1 s2 eff. KnownNat n
       => Ref s1 (Array n (Stored Uint8))
       -> Ref s2 (Array n (Stored Uint8))
       -> Ivory eff IxRep
memCmp a1 a2 = call memcmp (toCArray a1)
                           (toCArray a2)
                           (fromInteger $ fromTypeNat (aNat :: NatType n))


memCpy :: forall n s1 s2 eff. KnownNat n
       => Ref s1 (Array n (Stored Uint8))
       -> Ref s2 (Array n (Stored Uint8))
       -> Ivory eff IxRep
memCpy a1 a2 = call memcpy (toCArray a1)
                           (toCArray a2)
                           (fromInteger $ fromTypeNat (aNat :: NatType n))
