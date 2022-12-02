{-# LANGUAGE GADTs #-}

module Formula where

import           Feature
import           Interface.Timer

data Formula where
  Formula :: Timer t
          => { systemClock :: t
             , features    :: Features
             }
          -> Formula
