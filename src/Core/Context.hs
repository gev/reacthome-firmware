{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Core.Context where

import           Control.Monad.Writer
import           Core.Task
import           Ivory.Language
import           Ivory.Language.Module



data Context = Context
    { getModule :: ModuleM ()
    , getInits  :: [Def ('[] :-> ())]
    , getSteps  :: [Step]
    }



class Include a where
    include :: Monad m => a -> WriterT Context m ()



instance Include [ModuleM ()] where
    include = mapM_ include

instance Include (ModuleM ()) where
    include a = tell $ Context a mempty mempty



instance Include [Def ('[] :-> ())] where
    include a = do
        tell $ Context mempty a mempty
        include $ incl <$> a

instance Include (Def ('[] :-> ())) where
    include a = include [a]



instance Include [Step] where
    include a = do
        tell $ Context mempty mempty a
        include $ runStep <$> a

instance Include Step where
    include a = include [a]



instance Semigroup Context where
  (Context m1 i1 s1) <> (Context m2 i2 s2) = Context (m1 <> m2) (i1 <> i2) (s1 <> s2)

instance Monoid Context where
  mempty = Context mempty mempty mempty
