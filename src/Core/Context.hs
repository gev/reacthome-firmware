{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Core.Context where

import           Control.Monad.Writer
import           Core.Task
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Module



data Context = Context
    { getModule :: ModuleM ()
    , getInits  :: [Def ('[] :-> ())]
    , getTasks  :: [Task]
    }



addContext :: MonadWriter Context m => Context -> m ()
addContext = tell


addModule :: MonadWriter Context m => ModuleM () -> m ()
addModule m = tell $ Context m mempty mempty


addInit :: MonadWriter Context m => Def ('[] :-> ()) -> m ()
addInit d = do
    tell $ Context mempty [d] mempty
    addProc d


addTask :: MonadWriter Context m => Task -> m ()
addTask t = do
        tell $ Context mempty mempty [t]
        addProc $ runTask t


addProc :: MonadWriter Context m => Def ('[] :-> ()) -> m ()
addProc = addModule . incl


addArea :: (MonadWriter Context m, IvoryArea area) => MemArea area -> m ()
addArea = addModule . defMemArea


addStruct :: (MonadWriter Context m, IvoryStruct sym) => Proxy sym -> m ()
addStruct = addModule . defStruct



instance Semigroup Context where
  (Context m1 i1 t1) <> (Context m2 i2 t2) = Context (m1 <> m2) (i1 <> i2) (t1 <> t2)

instance Monoid Context where
  mempty = Context mempty mempty mempty
