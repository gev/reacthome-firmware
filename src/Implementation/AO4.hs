module Implementation.AO4 where

import Core.Actions
import Core.Controller
import Feature.CBM53D04
import Ivory.Language
import Ivory.Stdlib

newtype AO4 = AO4
    {aoutput4 :: CBM53D04}

ao4 :: (Monad m) => m t -> (t -> m CBM53D04) -> m AO4
ao4 transport' sbm53d04' = do
    transport <- transport'
    aoutput4 <- sbm53d04' transport
    pure AO4{aoutput4}

instance Controller AO4 where
    handle AO4{..} buff size = do
        action <- deref $ buff ! 0
        cond_
            [ action ==? actionDo ==> onDo aoutput4 buff size
            , action ==? actionDim ==> onDim aoutput4 buff size
            , action ==? actionInitialize ==> onInit aoutput4 buff size
            , action ==? actionGetState ==> forceSync aoutput4
            ]
