module Data.DoubleBuffer where

import Control.Monad.State
import Core.Context
import Data.Buffer
import Data.Value
import GHC.TypeLits
import Ivory.Language
import Ivory.Stdlib

data DoubleBuffer n t = DoubleBuffer
    { num0 :: Value Uint8
    , num1 :: Value Uint8
    , buff0 :: Buffer n t
    , buff1 :: Buffer n t
    }

doubleBuffer :: (MonadState Context m, KnownNat n, IvoryZeroVal t) => String -> m (DoubleBuffer n t)
doubleBuffer id = do
    num0 <- value (id <> "_double_num_0") 1
    num1 <- value (id <> "_double_num_1") 0
    buff0 <- buffer (id <> "_double_buff_0")
    buff1 <- buffer (id <> "_double_buff_1")
    pure DoubleBuffer{num0, num1, buff0, buff1}

selectDmaBuff ::
    (KnownNat n) =>
    DoubleBuffer n t ->
    (Buffer n t -> Ivory (ProcEffects s ()) ()) ->
    Ivory (ProcEffects s ()) ()
selectDmaBuff doubleBuff callback = do
    num0' <- deref $ num0 doubleBuff
    ifte_
        (num0' ==? 0)
        (callback (buff1 doubleBuff))
        (callback (buff0 doubleBuff))
    store (num0 doubleBuff) $ 1 - num0'

selectHandlerBuff ::
    (KnownNat n) =>
    DoubleBuffer n t ->
    (Buffer n t -> Ivory (ProcEffects s ()) ()) ->
    Ivory (ProcEffects s ()) ()
selectHandlerBuff doubleBuff callback = do
    num1' <- deref $ num1 doubleBuff
    num0' <- deref $ num0 doubleBuff
    when (num1' /=? num0') do
        ifte_
            (num1' ==? 0)
            (callback (buff0 doubleBuff))
            (callback (buff1 doubleBuff))
        store (num1 doubleBuff) num0'

lengthDoubleArray ::
    (KnownNat n, IvoryType t) =>
    DoubleBuffer n t ->
    Uint32
lengthDoubleArray dbuff = arrayLen (buff0 dbuff)
