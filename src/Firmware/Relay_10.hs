module Firmware.Relay_10 where

import           Device.Feature

relay :: [Feature Int]
relay =
  [ RBUS Slave  1
  , Relay      10
  , Findme      1
  , Service     1
  ]
