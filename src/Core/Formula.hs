{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Core.Formula where

import           Control.Monad.Reader  (Reader, runReader)
import           Control.Monad.Writer  (runWriter)
import           Core.Context
import           Core.Domain
import           Core.Feature
import           Core.Scheduler        (schedule, scheduler)
import           Core.Task
import           Core.Transport
import           Data.Foldable         (traverse_)
import           Data.Record
import           Data.Value
import           Interface.MCU
import           Ivory.Language
import           Ivory.Language.Module
import           Protocol.RBUS.Slave   (Slave (model))


data Formula where
    Formula :: Transport t
            => { model      :: Uint8
               , version    :: (Uint8, Uint8)
               , mcu        :: MCU p
               , shouldInit :: IBool
               , transport  :: Reader (Domain p t) t
               , features   :: [Reader (Domain p t) Feature]
               } -> Formula

cook :: Formula -> ModuleM ()
cook (Formula model version mcu shouldInit transport features) = do

    inclModule
    traverse_ (incl . runStep) tasks'
    incl  init
    incl  loop
    incl  main

    where domain'    = domain model version mcu shouldInit transport' features'
          transport' = runReader transport domain'
          features'  = (`runReader` domain') <$> features
          tasks'     = tasks transport' <> concatMap tasks features'
          scheduler' = scheduler (systemClock mcu) tasks'

          context    = snd (runWriter $ include domain')
                    <> snd (runWriter $ include scheduler')
                    <> snd (runWriter $ include features')

          inits      = getInits context
          inclModule = getModule context

          loop       = schedule scheduler'

          init :: Def ('[] :-> ())
          init = proc "init" $ body $ mapM_ call_ inits

          main :: Def ('[] :-> Sint32)
          main = proc "main" $ body $ call_ init >> call_ loop >> ret 0
