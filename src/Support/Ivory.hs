{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Support.Ivory where

import           Data.Foldable
import           Ivory.Language
import           Ivory.Language.Proc
import           Ivory.Language.Syntax
import           Ivory.Language.Syntax.Concrete.ParseAST (IncludeDef (inclDefLoc))


type HeaderFile = String

type Cast a e = a -> e


funFrom :: ProcType t => HeaderFile -> Sym -> Def t
funFrom = flip importProc

class (Bounded a, Enum a, Show a, IvoryExpr e) => ExtCast a e where
  defFrom :: HeaderFile -> a -> e
  defFrom h = (`extern` h) . show

  inclDef :: Cast a e -> ModuleDef
  inclDef c = traverse_ (inclSym . c) [minBound .. maxBound]

include :: HeaderFile
        -> ( ExtCast a e => Cast a e
           , ProcType t => Sym -> Def t
           )
include h = (defFrom h, funFrom h)
