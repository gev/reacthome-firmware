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
import           Scheduler             (schedule, scheduler)
import           Util.Data.Record
import qualified Util.Version          as V


data Formula where
    Formula :: MCU mcu
           => { version  :: (Uint8, Uint8)
              , mcu      :: mcu
              , features :: [Reader mcu Feature]
              } -> Formula

cook :: Formula -> ModuleM ()
cook (Formula (major, minor) mcu features) = do

    let fts    = (`runReader` mcu) <$> features
    let sch    = scheduler (systemClock mcu) $ concatMap tasks fts

    let inits  = initialize sch
              <> initialize (mac mcu)
              <> (initialize =<< fts)

    let init   = proc "init"
               $ body
               $ mapM_ call_ inits
              :: Def ('[] :-> ())

    let loop   = schedule sch

    let main   = proc "main"
               $ body
               $ call_ init
              >> call_ loop
              >> ret 0
              :: Def ('[] :-> Sint32)

    include   (V.version major minor)
    include   (mac mcu)
    include   sch

    traverse_ incl inits
    traverse_ include fts

    incl init
    incl loop
    incl main
