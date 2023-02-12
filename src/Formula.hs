{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Formula where

import           Data.Foldable
import           Feature
import           Include
import           Initialize
import           Interface.Mac
import           Interface.SystemClock
import           Ivory.Language
import           Ivory.Language.Module
import           Scheduler             (schedule, scheduler)

data Formula = Formula
    { mac      :: Mac
    , clock    :: SystemClock
    , features :: [Feature]
    }

cook :: Formula -> ModuleM ()
cook (Formula mac clock features) = do

    let sch       = scheduler clock $ concatMap tasks features

    let inits     = initialize sch
                 <> initialize mac
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
    include      mac


    incl init
    incl loop
    incl main
