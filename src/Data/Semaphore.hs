{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Semaphore where

import Control.Monad.State (MonadState)
import Core.Context
import Data.Value
import Ivory.Language
import Ivory.Stdlib

newtype Semaphore t = Semaphore {getSemaphore :: Value t}

semaphore ::
    (MonadState Context m, IvoryZeroVal t, IvoryInit t) =>
    String ->
    t ->
    m (Semaphore t)
semaphore id n = do
    v <- value (id <> "_semaphore") n
    pure $ Semaphore v

up ::
    (IvoryStore t, Num t) =>
    Semaphore t ->
    Ivory eff ()
up (Semaphore s) = do
    v <- deref s
    store s $ v + 1

down ::
    (IvoryStore t, IvoryOrd t, Num t) =>
    Semaphore t ->
    Ivory eff () ->
    Ivory eff ()
down (Semaphore s) run = do
    v <- deref s
    when (v >? 0) $ do
        store s $ v - 1
        run

down' ::
    (IvoryStore t, IvoryOrd t, Num t) =>
    Semaphore t ->
    Ivory eff () ->
    Ivory eff () ->
    Ivory eff ()
down' (Semaphore s) runT runF = do
    v <- deref s
    ifte_
        (v >? 0)
        ( do
            store s $ v - 1
            runT
        )
        runF

check ::
    (IvoryStore t, IvoryOrd t, Num t) =>
    Semaphore t ->
    Ivory eff () ->
    Ivory eff ()
check (Semaphore s) run = do
    v <- deref s
    when (v >? 0) run

check' ::
    (IvoryStore t, IvoryOrd t, Num t) =>
    Semaphore t ->
    Ivory eff () ->
    Ivory eff () ->
    Ivory eff ()
check' (Semaphore s) runT runF = do
    v <- deref s
    ifte_ (v >? 0) runT runF
