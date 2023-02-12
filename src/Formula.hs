{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Formula where

import           Data.Foldable
import           Feature
import           Include
import           Initialize
import           Interface.MCU
import           Ivory.Language
import           Ivory.Language.Module
import           Scheduler             (schedule, scheduler)

data Formula where
    Formula :: MCU mcu
           => { mcu      :: mcu
              , features :: [Feature]
              } -> Formula

cook :: Formula -> ModuleM ()
cook (Formula mcu features) = do

    let sch       = scheduler (systemClock mcu) $ concatMap tasks features

    let inits     = initialize sch
                 <> initialize (mac mcu)
                 <> (initialize =<< features)

    let init      = proc "init"
                  $ body
                  $ mapM_ call_ inits
                 :: Def ('[] :-> ())

    let loop      = schedule sch

    let main      = proc "main"
                  $ body
                  $ call_ init
                 >> call_ loop
                 >> ret 0
                 :: Def ('[] :-> Sint32)

    traverse_    incl inits
    traverse_    include features
    include      sch
    include      (mac mcu)


    incl init
    incl loop
    incl main
