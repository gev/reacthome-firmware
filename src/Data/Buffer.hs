module Data.Buffer where

import           Control.Monad.Writer
import           Core.Context
import           Data.Value
import           GHC.TypeNats
import           Ivory.Language


type Buffer n t = Values n t


buffer :: (Monad m, IvoryZeroVal t, IvoryInit t, KnownNat n)
       => String -> WriterT Context m (Buffer n t)
buffer id = do
    include $ defMemArea buff
    pure    buff
    where   buff = values_ $ id <> "_buffer"
