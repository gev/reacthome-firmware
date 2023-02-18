{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Core.Formula where

import           Control.Monad.Reader  (Reader, runReader)
import           Core.Domain
import qualified Core.Domain           as D
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
               , transport :: Reader (Domain mcu) t
               , features  :: [Reader (Domain mcu) Feature]
               } -> Formula

cook :: Formula -> ModuleM ()
cook (Formula model version mcu transport features) = do

    let domain   = D.domain model version mcu

    let fts      = (`runReader` domain) <$> features
    let tsk      = concatMap tasks fts
    let sch      = scheduler (systemClock mcu) tsk

    let inits    = initialize domain
                <> initialize sch
                <> (initialize =<< fts)

    let init     = proc "init"
                 $ body
                 $ mapM_ call_ inits
                :: Def ('[] :-> ())

    let loop     = schedule sch

    let main     = proc "main"
                 $ body
                 $ call_ init
                >> call_ loop
                >> ret 0
                :: Def ('[] :-> Sint32)

    include     domain
    traverse_   incl inits
    include     sch
    traverse_   include fts
    traverse_   (incl . runStep) tsk
    incl        init
    incl        loop
    incl        main
