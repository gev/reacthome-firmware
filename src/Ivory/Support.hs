{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Ivory.Support where

import           Ivory.Language
import           Ivory.Language.Proc
import           Ivory.Language.Syntax
import           Ivory.Language.Type


type HeaderFile = String


funFrom :: ProcType f => HeaderFile -> Sym -> Def f
funFrom = flip importProc


extFrom :: IvoryExpr e => HeaderFile -> Sym -> e
extFrom = flip extern



class IvoryVar e => ExtSymbol e where
    symbol :: e -> Sym
    symbol = sym . unwrapExpr
        where sym (ExpExtern (Extern {..})) = externSym
              sym e = error $ "Can't get a symbol of the expression: " <> show e
