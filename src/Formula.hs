{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}

module Formula where

import           Data.Foldable
import           Data.List
import           Feature
import           Interface             as I
import           Interface.SystemClock as I
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

  let depends = I.dependencies scheduler
            <> (I.dependencies =<< features)

  let inits   = I.initialize scheduler
            <> (I.initialize =<< features)

  let init    = proc "init"
              $ body
              $ mapM_ call_ inits
             :: Def ('[] :-> ())

  let loop    = schedule scheduler

  let main    = proc "main"
              $ body
              $ call_ init
             >> call_ loop
             >> ret 0
             :: Def ('[] :-> Sint32)

  sequenceA_ depends
  mapM_ incl inits
  incl init
  incl loop
  incl main
