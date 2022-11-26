module Device.Feature where

data Dim
  = AC
  | DC

data RBUS
  = Master
  | Slave

data Feature    a
  = AO          a
  | Button      a
  | ButtonL Int a
  | Dim     Dim a
  | Doppler     a
  | Eth         a
  | In          a
  | Findme      a
  | OW          a
  | Relay       a
  | RBUS   RBUS a
  | RS485       a
  | Service     a
