module Ivory.Support.Device.GD32F3x0 where

import           Ivory.Language
import           Ivory.Language.Proc
import           Ivory.Language.Syntax
import           Ivory.Support


headerFile :: HeaderFile
headerFile = "gd32f3x0.h"


ext :: IvoryExpr e => Sym -> e
ext  = extFrom headerFile


fun :: ProcType f => Sym -> Def f
fun = funFrom headerFile
