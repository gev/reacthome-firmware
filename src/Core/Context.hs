{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Core.Context where

import           Control.Monad.Writer
import           Ivory.Language
import           Ivory.Language.Module



data Context = Context
    { getModule :: ModuleM ()
    , getInits  :: [Def ('[] :-> ())]
    }



class Include a where
    include :: a -> Writer Context ()


instance Include (ModuleM ()) where
    include a = tell $ Context a mempty


instance Include [Def ('[] :-> ())] where
    include a = do
        tell $ Context mempty a
        mapM_ (include . incl) a

instance Include (Def ('[] :-> ())) where
    include a = include [a]



instance Semigroup Context where
  (Context m1 i1) <> (Context m2 i2) = Context (m1 <> m2) (i1 <> i2)


instance Monoid Context where
  mempty = Context mempty mempty
