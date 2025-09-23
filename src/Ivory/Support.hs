module Ivory.Support where

import Ivory.Language
import Ivory.Language.Init as I
import Ivory.Language.Proc
import Ivory.Language.Syntax
import Ivory.Language.Type

type HeaderFile = String

funFrom :: (ProcType f) => HeaderFile -> Sym -> Def f
funFrom = flip importProc

extFrom :: (IvoryExpr e) => HeaderFile -> Sym -> e
extFrom = flip extern

class (IvoryVar e) => ExtSymbol e where
  symbol :: e -> Sym
  symbol = sym . unwrapExpr
   where
    sym (ExpExtern (Extern{..})) = externSym
    sym e =
      error $
        "Can't get a symbol of the expression: "
          <> show e

(<+>) :: [I.InitStruct s] -> [I.InitStruct s] -> [I.InitStruct s]
a <+> b = a <> filter (labels a) b
 where
  labels a i = label i `notElem` (label <$> a)
  label (I.InitStruct [(l, _)]) = l
