{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}

module Formula where

import           Data.Foldable
import           Feature
import           Include
import           Initialize
import           Interface.SystemClock
import           Ivory.Language
import           Ivory.Language.Module
import           Scheduler             (Scheduler (Scheduler), schedule)

data Formula = Formula
  { clock    :: SystemClock
  , features :: [Feature]
  }

cook :: Formula -> ModuleM ()
cook (Formula {features, clock}) = do

  let scheduler = Scheduler clock $ concatMap tasks features

  let inits     = (initialize =<< features)
               <>  initialize scheduler

  let init      = proc "init"
                $ body
                $ mapM_ call_ inits
               :: Def ('[] :-> ())

  let loop      = schedule scheduler

  let main      = proc "main"
                $ body
                $ call_ init
               >> call_ loop
               >> ret 0
               :: Def ('[] :-> Sint32)

  traverse_  incl inits
  traverse_  include features
  include    scheduler


  incl init
  incl loop
  incl main
