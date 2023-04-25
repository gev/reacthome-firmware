{-# LANGUAGE GADTs #-}

module Core.Formula where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Domain
import           Core.Feature
import           Core.Transport
import           Interface.MCU
import           Ivory.Language
import           Ivory.Language.Module



{-
    TODO: abstract Formula to the type class
-}
data Formula p where
    Formula :: { name       ::  String
               , model      ::  Uint8
               , version    :: (Uint8,  Uint8)
               , shouldInit ::  IBool
               , transport  ::  WriterT Context (Reader (Domain p t)) t
               , features   :: [WriterT Context (Reader (Domain p t)) Feature]
               } -> Formula p
