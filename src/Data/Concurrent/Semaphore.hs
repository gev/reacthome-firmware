module Data.Concurrent.Semaphore where

import Data.Concurrent.Atomically
import Data.Semaphore
import Ivory.Language
import Ivory.Stdlib

upConcurrently ::
    (IvoryStore t, Num t) =>
    Semaphore t ->
    Ivory eff ()
upConcurrently = atomically . up

downConcurrently ::
    (IvoryStore t, IvoryOrd t, Num t) =>
    Semaphore t ->
    Ivory eff () ->
    Ivory eff ()
downConcurrently (Semaphore s) run = do
    v <- deref s
    when (v >? 0) $ do
        atomically $ do
            v' <- deref s
            store s $ v' - 1
        run

downConcurrently' ::
    (IvoryStore t, IvoryOrd t, Num t) =>
    Semaphore t ->
    Ivory eff () ->
    Ivory eff () ->
    Ivory eff ()
downConcurrently' (Semaphore s) runT runF = do
    v <- deref s
    flip (ifte_ $ v >? 0) runF $ do
        atomically $ do
            v' <- deref s
            store s $ v' - 1
        runT
