{-# LANGUAGE GADTs #-}

module Core.Feature where

import           Core.Controller
import           Core.Include
import           Core.Initialize
import           Core.Task
import           Ivory.Language


data Feature where
    Feature :: (Task f, Controller f) => f -> Feature


instance Include Feature where
    include (Feature f) = include f

instance Initialize Feature where
    initialize (Feature f) = initialize f

instance Task Feature where
    tasks (Feature f) = tasks f

instance Controller Feature where
    handle (Feature f) = handle f
