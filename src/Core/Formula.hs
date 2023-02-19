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

    include     dmn
    traverse_   incl inits
    include     sch
    traverse_   include fts
    traverse_   (incl . runStep) tsk
    incl        init
    incl        loop
    incl        main

    where dmn    = domain model version mcu trp
          trp    = runReader transport dmn
          fts    = (`runReader` dmn) <$> features
          tsk    = tasks trp <> concatMap tasks fts
          sch    = scheduler (systemClock mcu) tsk
          loop   = schedule sch
          inits  = initialize dmn <> initialize sch <> (initialize =<< fts)

          init  :: Def ('[] :-> ())
          init   = proc "init" $ body $ mapM_ call_ inits

          main  :: Def ('[] :-> Sint32)
          main   = proc "main" $ body $ call_ init >> call_ loop >> ret 0
