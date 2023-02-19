module Core.Dispatcher where

import           Core.Controller
import           Data.Buffer
import           Ivory.Language
import           Ivory.Stdlib

makeDispatcher :: Controller c
               => [c]
               -> Buffer l t -> n -> Ivory (ProcEffects s ()) ()
makeDispatcher controllers buff n = cond_ conditions
    where
        conditions = concatMap run handlers
        handlers   = handle <$> controllers
        run h      = h buff n
