{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Core.Context where

import           Control.Monad.Writer
import           Core.Task
import           Data.List
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Module



data Context = Context
    { getModule :: ModuleDef
    , getInits  :: [Def ('[] :-> ())]
    , getTasks  :: [Task]
    }



addContext :: MonadWriter Context m => Context -> m ()
addContext = tell


addModule :: MonadWriter Context m => ModuleDef -> m ()
addModule m = addContext $ Context m mempty mempty


addInit :: MonadWriter Context m => String -> (forall s. Ivory (ProcEffects s ()) ()) -> m (Def ('[] :-> ()))
addInit id run = do
    addContext $ Context mempty [d] mempty
    addProc d
    pure d
    where d = proc (id <> "_init") $ body run


addTask :: MonadWriter Context m => Task -> m ()
addTask t = do
        addContext $ Context mempty mempty [t]
        addProc $ getTask t


addProc :: MonadWriter Context m => Def p -> m ()
addProc = addModule . incl


addArea :: (MonadWriter Context m, IvoryArea area) => MemArea area -> m ()
addArea = addModule . defMemArea


addConstArea :: (MonadWriter Context m, IvoryArea area) => ConstMemArea area -> m ()
addConstArea = addModule . defConstMemArea


addStruct :: (MonadWriter Context m, IvoryStruct sym) => Proxy sym -> m ()
addStruct = addModule . defStruct



instance Semigroup Context where
  (Context m1 i1 t1) <> (Context m2 i2 t2) = Context (m1 <> m2) (nub $ i1 <> i2) (t1 <> t2)

instance Monoid Context where
  mempty = Context mempty mempty mempty
