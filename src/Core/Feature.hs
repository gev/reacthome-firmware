{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}

module Core.Feature where

import           Core.Context
import           Core.Controller
import           Core.Task
import           Ivory.Language


data Feature where
    Feature :: (Task f, Controller f) => f -> Feature


instance Include Feature where
    include (Feature f) = include f

instance Include [Feature] where
    include = mapM_ include

instance Task Feature where
    tasks (Feature f) = tasks f

instance Controller Feature where
    handle (Feature f) = handle f
