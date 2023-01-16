{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ivory.Support where

import           Data.Foldable
import           Ivory.Language
import           Ivory.Language.Proc
import           Ivory.Language.Syntax


type HeaderFile = String

type Cast a e = a -> e


funFrom :: ProcType t => HeaderFile -> Sym -> Def t
funFrom = flip importProc

class (Bounded a, Enum a, Show a, IvoryExpr e) => ExtDef a e where
    defFrom :: HeaderFile -> Cast a e
    defFrom h = (`extern` h) . show

    inclDef :: Cast a e -> ModuleDef
    inclDef c = traverse_ (inclSym . c) [minBound .. maxBound]

include :: HeaderFile
        -> ( ExtDef a e => Cast a e
           , ProcType t => Sym -> Def t
           )
include h = (defFrom h, funFrom h)
