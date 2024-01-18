{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Implementation.Echo where

import           Control.Monad.Reader
import           Control.Monad.State
import           Core.Context
import           Core.Controller
import           Core.Domain
import qualified Core.Domain          as D
import           Core.Task
import qualified Core.Transport       as T
import           Data.Buffer
import           Data.Value
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Stdlib


data Echo = Echo
    { buff     :: Buffer 10 Uint8
    , transmit :: forall n s t. KnownNat n
               => Buffer n Uint8
               -> Ivory (ProcEffects s t) ()
    }


echo :: ( MonadState Context m
        , MonadReader (Domain p t Echo) m
        , T.Transport t
        ) => m Echo
echo = do
    buff <- values "echo_buffer" [9,8,7,6,5,4,3,2,1,0]
    transport  <- asks D.transport
    let echo = Echo { buff, transmit = T.transmitBuffer transport }
    -- addTask $ echoTask echo
    pure echo


echoTask :: Echo -> Task
echoTask Echo{..} = delay 100 "echo_tx" $ transmit buff


instance Controller Echo where
    handle Echo{..} request n = transmit request