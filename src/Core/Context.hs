{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Core.Context where

import           Control.Monad.State
import           Core.Task
import           Data.List
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Module



data Context = Context
    { getModule :: ModuleDef
    , getInits  :: [Def ('[] :-> ())]
    , getTasks  :: [Task]
    , getSyncs  :: [Def ('[] :-> ())]
    }



addContext :: MonadState Context m => Context -> m ()
addContext context = modify (<> context)


addModule :: MonadState Context m => ModuleDef -> m ()
addModule m = addContext $ Context m mempty mempty mempty


addInit :: MonadState Context m => String -> (forall s. Ivory (ProcEffects s ()) ()) -> m (Def ('[] :-> ()))
addInit id run = do
    addContext $ Context mempty [d] mempty mempty
    addProc d
    pure d
    where d = proc (id <> "_init") $ body run


addTask :: MonadState Context m => Task -> m ()
addTask t = do
        addContext $ Context mempty mempty [t] mempty
        addProc $ getTask t


addSync :: MonadState Context m => String -> (forall eff. Ivory eff ()) -> m ()
addSync id run = do
      addContext $ Context mempty mempty mempty [d]
      addProc d
      where d = proc (id <> "_sync") $ body run


addProc :: MonadState Context m => Def p -> m ()
addProc = addModule . incl


addArea :: (MonadState Context m, IvoryArea area) => MemArea area -> m ()
addArea = addModule . defMemArea


addConstArea :: (MonadState Context m, IvoryArea area) => ConstMemArea area -> m ()
addConstArea = addModule . defConstMemArea


addStruct :: (MonadState Context m, IvoryStruct sym) => Proxy sym -> m ()
addStruct = addModule . defStruct



instance Semigroup Context where
  (Context m1 i1 t1 s1) <> (Context m2 i2 t2 s2) =
    Context (m1 <> m2) (i1 <> i2) (nub $ t1 <> t2) (s1 <> s2)

instance Monoid Context where
  mempty = Context mempty mempty mempty mempty
