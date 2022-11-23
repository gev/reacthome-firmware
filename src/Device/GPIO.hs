module Device.GPIO where

data IO'
  = IN
  | OUT

data UART'
  = RX
  | TX
  | RE_DE

data I2C'
  = SCL
  | SDA

data SPI'
  = SCKL
  | MISO
  | MOSI
  | SS

data GPIO
  = IO    Int IO'
  | I2C   Int I2C'
  | SPI   Int SPI'
  | UART  Int UART'

