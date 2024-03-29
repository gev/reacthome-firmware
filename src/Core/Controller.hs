{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Core.Controller where

import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Array
import           Ivory.Stdlib


class Controller c where
    handle :: KnownNat l
           => c
           -> Buffer l Uint8
           -> Uint8
           -> Ivory (ProcEffects s t) ()
    handle _ _ _ = cond_ []

instance Controller ()
