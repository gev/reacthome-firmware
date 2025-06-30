{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.DoubleBuffer where
    
import           Control.Monad.State
import           Core.Context
import           Data.Value
import           Ivory.Language
import           GHC.TypeLits
import           Data.Buffer 
import           Ivory.Stdlib

data DoubleBuffer n t = DoubleBuffer
    { exchangeNum :: Value Uint8
    , prepareNum  :: Value Uint8
    , buff0       :: Buffer n t
    , buff1       :: Buffer n t
    }


doubleBuffer :: (MonadState Context m,  KnownNat n, IvoryZeroVal t) => String -> m (DoubleBuffer n t)
doubleBuffer id = do
    exchangeNum <- value   ( id <> "_exchange_num"  )  1
    prepareNum  <- value   ( id <> "_prepare_num"   )  0
    buff0       <- buffer  ( id <> "_double_buff_0" ) 
    buff1       <- buffer  ( id <> "_double_buff_1" )
    pure DoubleBuffer {exchangeNum, prepareNum, buff0, buff1}



preBuff :: KnownNat n => DoubleBuffer n t -> (Buffer n t -> Ivory (ProcEffects s ()) ()) -> Ivory (ProcEffects s ()) ()
preBuff doubleBuff callback= do
    prepareNum' <- deref $ prepareNum doubleBuff
    numExchangeBuff' <- deref $ exchangeNum doubleBuff
    when (prepareNum' /=? numExchangeBuff') $ do
        ifte_ (prepareNum' ==? 0)
            (callback (buff0 doubleBuff))
            (callback (buff1 doubleBuff))
        store (prepareNum doubleBuff) numExchangeBuff'


exchangeDBuff :: KnownNat n => DoubleBuffer n t -> (Buffer n t -> Ivory (ProcEffects s ()) ()) -> Ivory (ProcEffects s ()) ()
exchangeDBuff doubleBuff callback = do
    exchangeNum' <- deref $ exchangeNum doubleBuff
    ifte_ (exchangeNum' ==? 0)
        (callback (buff1 doubleBuff))
        (callback (buff0 doubleBuff))
    store (exchangeNum doubleBuff) $ 1 - exchangeNum'


lengthDoubleArray :: (KnownNat n, IvoryType t) => DoubleBuffer n t -> Uint32
lengthDoubleArray dbuff = arrayLen (buff0 dbuff)