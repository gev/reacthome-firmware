{-# LANGUAGE DataKinds #-}

module Core.FSM where

import Ivory.Language
import Ivory.Stdlib

transit :: a -> b -> (a, b)
transit = (,)

(|->) = transit

runState ::
    (IvoryStore a, IvoryEq a) =>
    (t -> Ref s (Stored a)) ->
    [(a, t -> p -> Ivory eff ())] ->
    t ->
    p ->
    Ivory eff ()
runState f ts st i = do
    p <- deref (f st)
    let run (w, h) = w ==? p ==> h st i
    cond_ $ run <$> ts

runState' ::
    (IvoryStore a, IvoryEq a) =>
    (t -> Ref s (Stored a)) ->
    [(a, t -> Ivory eff ())] ->
    t ->
    Ivory eff ()
runState' f ts st = do
    p <- deref (f st)
    let run (w, h) = w ==? p ==> h st
    cond_ $ run <$> ts

runInput ::
    (IvoryEq p) =>
    [(p, t -> p -> Ivory eff ())] ->
    t ->
    p ->
    Ivory eff ()
runInput ts st i =
    cond_ $ run <$> ts
  where
    run (w, h) = w ==? i ==> h st i
