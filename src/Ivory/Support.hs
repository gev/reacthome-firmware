{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ivory.Support where

import           Ivory.Language
import           Ivory.Language.Proc
import           Ivory.Language.Syntax


type HeaderFile = String


funFrom :: ProcType f => HeaderFile -> Sym -> Def f
funFrom = flip importProc


extFrom :: IvoryExpr e => HeaderFile -> Sym -> e
extFrom = flip extern
