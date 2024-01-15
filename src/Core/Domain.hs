{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Core.Domain where

import           Control.Monad.State
import           Core.Context
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



data Domain p t i where
     Domain :: { model          :: Value Uint8
               , version        :: V.Version
               , mcu            :: I.MCU p
               , mustInit       :: IBool
               , shouldInit     :: Value IBool
               , transport      :: t
               , implementation :: i
               } -> Domain p t i



domain :: (MonadState Context m)
       => Uint8
       -> (Uint8, Uint8)
       -> I.MCU p
       -> IBool
       -> t
       -> i
       -> m (Domain p t i)
domain model' version' mcu mustInit transport implementation = do
    addModule inclCast
    addModule inclString
    addModule inclSerialize
    addModule inclReadAddr
    model      <- value "model" model'
    version    <- V.version "version" version'
    shouldInit <- value "should_init" mustInit
    pure Domain { model, version, mcu, mustInit, shouldInit, transport, implementation}
