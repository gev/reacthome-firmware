{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}

module Formula where

import           Data.Foldable
import           Feature
import           Interface             as I
import           Interface.IRQ         as Q
import           Interface.Timer
import           Ivory.Language
import           Ivory.Language.Module
import           Scheduler

data Formula where
  Formula :: Q.IRQ t
          => { systemClock :: t
             , features    :: [Feature]
             }
          -> Formula

cook :: Formula -> ModuleM ()
cook (Formula {features, systemClock}) = do


  let scheduler = Scheduler systemClock $ task <$> features

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
