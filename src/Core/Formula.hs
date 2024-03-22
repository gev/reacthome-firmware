{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Core.Formula where

import           Control.Monad.Reader
import           Control.Monad.State
import           Core.Context
import           Core.Domain
import           Ivory.Language
import           Ivory.Language.Module



data Formula p where
    Formula :: { name           ::  String
               , model          ::  Uint8
               , version        :: (Uint8, Uint8)
               , shouldInit     ::  IBool
               , implementation ::  StateT Context (Reader (Domain p i)) i
               } -> Formula p
