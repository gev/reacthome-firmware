{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}

module Core.Feature where

import           Core.Controller
import           Core.Task
import           Ivory.Language


data Feature where
    Feature :: Controller f => f -> Feature


instance Controller Feature where
    handle (Feature f) = handle f
