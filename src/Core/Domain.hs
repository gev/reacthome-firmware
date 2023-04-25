{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Core.Domain where

import           Control.Monad.Writer
import           Core.Context
import           Core.Feature
import           Core.Transport
import qualified Core.Version          as V
import           Data.Buffer
import           Data.Record
import           Data.Value
import qualified Interface.Mac         as M
import qualified Interface.MCU         as I
import           Interface.SystemClock
import           Ivory.Language
import           Support.Cast
import           Support.ReadAddr
import           Support.Serialize
import           Util.String



data Domain p t where
     Domain :: { model      :: Value Uint8
               , version    :: V.Version
               , mcu        :: I.MCU p
               , shouldInit :: Value IBool
               , transport  :: t
               , features   :: [Feature]
               } -> Domain p t



domain :: (MonadWriter Context m)
       => Uint8
       -> (Uint8, Uint8)
       -> I.MCU p
       -> IBool
       -> t
       -> [Feature]
       -> m (Domain p t)
domain model' version' mcu shouldInit' transport features = do
    addModule inclCast
    addModule inclString
    addModule inclSerialize
    addModule inclReadAddr
    model      <- value "model" model'
    version    <- V.version "version" version'
    shouldInit <- value "should_init" shouldInit'
    pure Domain { model, version, mcu, shouldInit, transport, features}
