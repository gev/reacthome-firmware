module Device.Feature where

data Dim
  = AC
  | DC

data RBUS
  = Master
  | Slave

data Feature          a
  = AO          Int   a
  | Button      Int   a
  | ButtonL Int Int   a
  | Dim     Dim Int   a
  | Doppler     Int   a
  | Eth         Int   a
  | In          Int   a
  | Findme      Int   a
  | OW          Int   a
  | Relay       Int   a
  | RBUS   RBUS Int   a
  | RS485       Int   a
  | Service     Int   a
