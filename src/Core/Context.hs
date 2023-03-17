{-# LANGUAGE DataKinds             #-}
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



class Include a where
    include :: Monad m => a -> WriterT Context m ()



instance Include [ModuleM ()] where
    include = include . mconcat

instance Include (ModuleM ()) where
    include a = tell $ Context a mempty mempty



instance Include [Def ('[] :-> ())] where
    include a = do
        tell $ Context mempty a mempty
        include $ incl <$> a

instance Include (Def ('[] :-> ())) where
    include :: Monad m => Def ('[] ':-> ()) -> WriterT Context m ()
    include a = include [a]



instance Include [Task] where
    include a = do
        tell $ Context mempty mempty a
        include $ runTask <$> a

instance Include Task where
    include a = include [a]




instance Semigroup Context where
  (Context m1 i1 t1) <> (Context m2 i2 t2) = Context (m1 <> m2) (i1 <> i2) (t1 <> t2)

instance Monoid Context where
  mempty = Context mempty mempty mempty
