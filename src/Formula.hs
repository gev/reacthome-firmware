{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Formula where

import           Control.Monad.Reader  (Reader, runReader)
import           Data.Foldable
import           Feature
import           Include
import           Initialize
import           Interface.MCU
import           Ivory.Language
import           Ivory.Language.Module
import           Protocol.RBUS.Slave   (Slave (model))
import           Scheduler             (schedule, scheduler)
import           Util.Data.Record
import           Util.Data.Value
import qualified Util.Version          as V


data Formula where
    Formula :: MCU mcu
           => { model    :: Uint8
              , version  :: (Uint8, Uint8)
              , mcu      :: mcu
              , features :: [Reader mcu Feature]
              } -> Formula

cook :: Formula -> ModuleM ()
cook (Formula model (major, minor) mcu features) = do

    let model'   = value "model" model
    let mac'     = mac mcu "mac"
    let version' = V.version "version" major minor

    let fts      = (`runReader` mcu) <$> features
    let tsk      = concatMap tasks fts
    let sch      = scheduler (systemClock mcu) tsk

    let inits    = initialize mac'
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

    include     model'
    include     version'
    include     mac'
    traverse_   incl inits
    include     sch
    traverse_   include fts
    traverse_   (incl . runStep) tsk
    incl        init
    incl        loop
    incl        main
