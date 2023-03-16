{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}

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
import           Support.Serialize



data Domain p t where
     Domain :: Transport t
            => { model      :: Value Uint8
               , version    :: V.Version
               , mcu        :: I.MCU p
               , shouldInit :: Value IBool
               , transport  :: t
               , features   :: [Feature]
               } -> Domain p t



domain :: (Monad m, Transport t)
       => Uint8
       -> (Uint8, Uint8)
       -> I.MCU p
       -> IBool
       -> t
       -> [Feature]
       -> WriterT Context m (Domain p t)
domain model' (major, minor) mcu shouldInit' transport features = do
    include inclCast
    include inclSerialize
    let model      = value "model" model'
    let version    = V.version "version" major minor
    let shouldInit = value "should_init" shouldInit'
    include model
    include version
    include shouldInit
    pure Domain { model, version, mcu, shouldInit, transport, features}
