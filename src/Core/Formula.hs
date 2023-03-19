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



data Formula where
    Formula :: Transport t
            => { model      ::  Uint8
               , version    :: (Uint8,  Uint8)
               , mcu        ::  Writer  Context (MCU p)
               , shouldInit ::  IBool
               , transport  ::  WriterT Context (Reader (Domain p t)) t
               , features   :: [WriterT Context (Reader (Domain p t)) Feature]
               } -> Formula
