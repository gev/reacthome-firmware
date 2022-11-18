{-# LANGUAGE MultiParamTypeClasses #-}
module Support.Ivory where

import           Data.Foldable
import           Ivory.Language
import           Ivory.Language.Proc
import           Ivory.Language.Syntax


type HeaderFile = String

type Ext a e = a -> e


extProcFrom :: ProcType t => HeaderFile -> Sym -> Def t
extProcFrom = flip importProc


class (Bounded a, Enum a, Show a, IvoryExpr e) => ExtConst a e where
  extConstFrom :: HeaderFile -> a -> e
  extConstFrom h = (`extern` h) . show

  inclConst :: Ext a e -> ModuleDef
  inclConst c = traverse_ (inclSym . c) [minBound .. maxBound]

