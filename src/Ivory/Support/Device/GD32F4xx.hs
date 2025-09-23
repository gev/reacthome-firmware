module Ivory.Support.Device.GD32F4xx where

import Ivory.Language
import Ivory.Language.Proc
import Ivory.Language.Syntax
import Ivory.Support

headerFile :: HeaderFile
headerFile = "gd32f4xx.h"

ext :: (IvoryExpr e) => Sym -> e
ext = extFrom headerFile

fun :: (ProcType f) => Sym -> Def f
fun = funFrom headerFile
