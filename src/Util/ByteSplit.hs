module Util.ByteSplit where

import           Ivory.Language


class ByteSplit n where
    bytes :: n -> [Uint8]

instance ByteSplit Uint16 where
    bytes n =  ($ n) <$> [ ubits
                         , lbits
                         ]

instance ByteSplit Uint32 where
    bytes n =  ($ n) <$> [ ubits . ubits
                         , lbits . ubits
                         , ubits . lbits
                         , lbits . lbits
                         ]
