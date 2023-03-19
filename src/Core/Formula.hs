{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Core.Formula where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Core.Context
import           Core.Domain
import           Core.Feature
import           Core.Scheduler
import           Core.Transport
import           Interface.MCU
import           Ivory.Language
import           Ivory.Language.Module



data Formula where
    Formula :: Transport t
            => { model      ::  Uint8
               , version    :: (Uint8,  Uint8)
               , mcu        ::  Writer  Context (MCU p)
               , shouldInit ::  IBool
               , transport  ::  WriterT Context (Reader (Domain p t)) t
               , features   :: [WriterT Context (Reader (Domain p t)) Feature]
               } -> Formula



cook :: Formula -> ModuleM ()
cook (Formula model version mcu shouldInit transport features) = do

    inclModule
    incl  init
    incl  loop
    incl  main

    where (domain'   , domainContext'    ) = runWriter $ domain model version mcu' shouldInit transport' features'
          (transport', transportContext' ) = runReader (runWriterT transport) domain'
          (features' , featuresContext'  ) = unzip $ run <$> features
          (mcu'      , mcuContext'       ) = runWriter mcu

          run f = runReader (runWriterT f) domain'

          (Context inclModule inits tasks) = mcuContext'
                                          <> domainContext'
                                          <> transportContext'
                                          <> mconcat featuresContext'

          loop = mkLoop (systemClock mcu') tasks

          init :: Def ('[] :-> ())
          init = proc "init" $ body $ mapM_ call_ inits

          main :: Def ('[] :-> Sint32)
          main = proc "main" $ body $ do
            call_ init
            call_ loop
            ret 0
