{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Core.Context where

import Control.Monad.State
import Core.Task
import Data.List
import GHC.TypeNats
import Ivory.Language
import Ivory.Language.Module

data Context = Context
    { getModule :: ModuleDef
    , getInits :: [Def ('[] :-> ())]
    , getTasks :: [Task]
    , getSyncs :: [Def ('[] :-> ())]
    , getBody :: forall s. [(String, Ivory (ProcEffects s ()) ())]
    }

addContext :: (MonadState Context m) => Context -> m ()
addContext context = modify (<> context)

addModule :: (MonadState Context m) => ModuleDef -> m ()
addModule m =
    addContext $
        Context
            m
            mempty
            mempty
            mempty
            mempty

addInit ::
    (MonadState Context m) =>
    String ->
    (forall s. Ivory (ProcEffects s ()) ()) ->
    m (Def ('[] :-> ()))
addInit id run = do
    addContext $
        Context
            mempty
            [d]
            mempty
            mempty
            mempty
    addProc d
    pure d
  where
    d = proc (id <> "_init") $ body run

addTask :: (MonadState Context m) => Task -> m ()
addTask t = do
    addContext $
        Context
            mempty
            mempty
            [t]
            mempty
            mempty
    addProc $ getTask t

addSync ::
    (MonadState Context m) =>
    String ->
    (forall eff. Ivory eff ()) ->
    m ()
addSync id run = do
    addContext $
        Context
            mempty
            mempty
            mempty
            [d]
            mempty
    addProc d
  where
    d = proc (id <> "_sync") $ body run

addBody ::
    (MonadState Context m) =>
    String ->
    (forall s. Ivory (ProcEffects s ()) ()) ->
    m ()
addBody id body = do
    addContext $
        Context
            mempty
            mempty
            mempty
            mempty
            [(id, body)]

addProc :: (MonadState Context m) => Def p -> m ()
addProc = addModule . incl

addArea :: (MonadState Context m, IvoryArea area) => MemArea area -> m ()
addArea = addModule . defMemArea

addConstArea :: (MonadState Context m, IvoryArea area) => ConstMemArea area -> m ()
addConstArea = addModule . defConstMemArea

addStruct :: (MonadState Context m, IvoryStruct sym) => Proxy sym -> m ()
addStruct = addModule . defStruct

instance Semigroup Context where
    (Context m1 i1 t1 s1 b1) <> (Context m2 i2 t2 s2 b2) =
        Context
            (m1 <> m2)
            (nub $ i1 <> i2)
            (nub $ t1 <> t2)
            (nub $ s1 <> s2)
            (b1 <> b2)

instance Monoid Context where
    mempty =
        Context
            mempty
            mempty
            mempty
            mempty
            mempty
