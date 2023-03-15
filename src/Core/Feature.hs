{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}

module Core.Feature where

import           Core.Context
import           Core.Controller
import           Core.Task
import           Ivory.Language


data Feature where
    Feature :: (Include f, Controller f) => f -> Feature


instance Include Feature where
    include (Feature f) = include f

instance Include [Feature] where
    include = mapM_ include

instance Controller Feature where
    handle (Feature f) = handle f
