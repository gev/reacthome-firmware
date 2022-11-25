module Device.Slot.Top where

data Pin_1  = IO_15_IN | IO_15_OUT | UART_5_X
data Pin_2  = IO_17_IN | IO_17_OUT | UART_6_RE_DE
data Pin_3  = IO_19_IN | IO_19_OUT | UART_6_X
data Pin_4  = IO_21_IN | IO_21_OUT | I2C_2_SCL
data Pin_5  = IO_23_IN | IO_23_OUT | SPI_1_SCKL
data Pin_6  = IO_25_IN | IO_25_OUT | SPI_1_MOSI
data Pin_7  = IO_27_IN | IO_27_OUT
data Pin_8  = IO_29_IN | IO_29_OUT
data Pin_9  = IO_31_IN | IO_31_OUT

data Pin_13 = IO_16_IN | IO_16_OUT | UART_5_RX
data Pin_14 = IO_18_IN | IO_18_OUT | UART_5_RE_DE
data Pin_15 = IO_20_IN | IO_20_OUT | UART_6_RX
data Pin_16 = IO_22_IN | IO_22_OUT | I2C_2_SDA
data Pin_17 = IO_24_IN | IO_24_OUT | SPI_1_MISO
data Pin_18 = IO_26_IN | IO_26_OUT | SPI_1_SS
data Pin_19 = IO_28_IN | IO_28_OUT
data Pin_20 = IO_30_IN | IO_30_OUT
data Pin_21 = IO_32_IN | IO_32_OUT

data Slot = Slot
  { pin_1  :: Pin_1
  , pin_2  :: Pin_2
  , pin_3  :: Pin_3
  , pin_4  :: Pin_4
  , pin_5  :: Pin_5
  , pin_6  :: Pin_6
  , pin_7  :: Pin_7
  , pin_8  :: Pin_8
  , pin_9  :: Pin_9

  , pin_13 :: Pin_13
  , pin_14 :: Pin_14
  , pin_15 :: Pin_15
  , pin_16 :: Pin_16
  , pin_17 :: Pin_17
  , pin_18 :: Pin_18
  , pin_19 :: Pin_19
  , pin_20 :: Pin_20
  , pin_21 :: Pin_21
  }
