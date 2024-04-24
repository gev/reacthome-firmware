{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Value
    ( Value
    , Values
    , Values'
    , value_
    , value
    , values_
    , values
    ) where

import           Control.Monad.State
import           Core.Context
import           Data.Area
import           GHC.TypeNats
import           Ivory.Language



type Value     t = Ref Global (Stored t)
type Values  n t = Ref Global (Array  n (Stored t))
type Values' n t = MemArea    (Array  n (Stored t))



value_ :: (MonadState Context m, IvoryZeroVal t)
       => String -> m (Value t)
value_ id = mkArea id Nothing

value :: (MonadState Context m, IvoryZeroVal t, IvoryInit t)
      => String -> t -> m (Value t)
value id v = mkArea id . Just $ ival v



values_ :: (MonadState Context m, KnownNat n, IvoryZeroVal t)
        => String -> m (Values n t)
values_ id = mkArea id Nothing

values :: (MonadState Context m, KnownNat n, IvoryZeroVal t, IvoryInit t)
       => String -> [t] -> m (Values n t)
values id v = mkArea id . Just . iarray $ ival <$> v
