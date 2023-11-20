{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Interface.LwipPort where

import           Ivory.Language
import           Core.Handler  
import           Support.Lwip.Err
import           Support.Lwip.Netif


class LwipPort p where
    initLwipPortIf  :: p -> ProcPtr ('[NETIF s] :-> ErrT)
    inputLwipPortIf :: p -> NETIF s -> Ivory eff ErrT
