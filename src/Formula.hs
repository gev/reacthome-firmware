{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Formula where

import           Control.Monad.Reader  (Reader, runReader)
import           Data.Foldable
import           Data.Record
import           Data.Value
import           Domain
import qualified Domain                as D
import           Feature
import           Include
import           Initialize
import           Interface.MCU
import           Ivory.Language
import           Ivory.Language.Module
import           Protocol.RBUS.Slave   (Slave (model))
import           Scheduler             (schedule, scheduler)


data Formula where
    Formula :: MCU mcu
            => { model    :: Uint8
               , version  :: (Uint8, Uint8)
               , mcu      :: mcu
               , features :: [Reader (Domain mcu) Feature]
               } -> Formula

cook :: Formula -> ModuleM ()
cook (Formula model version mcu features) = do

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
