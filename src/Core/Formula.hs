{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Core.Formula where

import           Control.Monad.Reader  (Reader, runReader)
import           Core.Domain
import           Core.Feature
import           Core.Include
import           Core.Initialize
import           Core.Scheduler        (schedule, scheduler)
import           Core.Task
import           Core.Transport
import           Data.Foldable
import           Data.Record
import           Data.Value
import           Interface.MCU
import           Ivory.Language
import           Ivory.Language.Module
import           Protocol.RBUS.Slave   (Slave (model))


data Formula where
    Formula :: (MCU mcu, Transport t)
            => { model     :: Uint8
               , version   :: (Uint8, Uint8)
               , mcu       :: mcu
               , transport :: Reader (Domain mcu t) t
               , features  :: [Reader (Domain mcu t) Feature]
               } -> Formula

cook :: Formula -> ModuleM ()
cook (Formula model version mcu transport features) = do

    include     domain'
    traverse_   incl inits
    include     scheduler'
    traverse_   include features'
    traverse_   (incl . runStep) tasks'
    incl        init
    incl        loop
    incl        main

    where domain'    = domain model version mcu transport'
          transport' = runReader transport domain'
          features'  = (`runReader` domain') <$> features
          tasks'     = tasks transport' <> concatMap tasks features'
          scheduler' = scheduler (systemClock mcu) tasks'
          inits      = initialize domain' <> initialize scheduler' <> (initialize =<< features')
          loop       = schedule scheduler'

          init :: Def ('[] :-> ())
          init = proc "init" $ body $ mapM_ call_ inits

          main :: Def ('[] :-> Sint32)
          main = proc "main" $ body $ call_ init >> call_ loop >> ret 0
