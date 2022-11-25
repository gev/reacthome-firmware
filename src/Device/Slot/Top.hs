module Device.Slot.Top where

data Pin_t1  = IO_15_IN | IO_15_OUT | UART_5_TX
data Pin_t2  = IO_17_IN | IO_17_OUT | UART_6_RE_DE
data Pin_t3  = IO_19_IN | IO_19_OUT | UART_6_TX
data Pin_t4  = IO_21_IN | IO_21_OUT | I2C_2_SCL
data Pin_t5  = IO_23_IN | IO_23_OUT | SPI_1_SCKL
data Pin_t6  = IO_25_IN | IO_25_OUT | SPI_1_MOSI
data Pin_t7  = IO_27_IN | IO_27_OUT
data Pin_t8  = IO_29_IN | IO_29_OUT
data Pin_t9  = IO_31_IN | IO_31_OUT

data Pin_t13 = IO_16_IN | IO_16_OUT | UART_5_RX
data Pin_t14 = IO_18_IN | IO_18_OUT | UART_5_RE_DE
data Pin_t15 = IO_20_IN | IO_20_OUT | UART_6_RX
data Pin_t16 = IO_22_IN | IO_22_OUT | I2C_2_SDA
data Pin_t17 = IO_24_IN | IO_24_OUT | SPI_1_MISO
data Pin_t18 = IO_26_IN | IO_26_OUT | SPI_1_SS
data Pin_t19 = IO_28_IN | IO_28_OUT
data Pin_t20 = IO_30_IN | IO_30_OUT
data Pin_t21 = IO_32_IN | IO_32_OUT

data Slot = Slot
  { pin_t1  :: Pin_t1
  , pin_t2  :: Pin_t2
  , pin_t3  :: Pin_t3
  , pin_t4  :: Pin_t4
  , pin_t5  :: Pin_t5
  , pin_t6  :: Pin_t6
  , pin_t7  :: Pin_t7
  , pin_t8  :: Pin_t8
  , pin_t9  :: Pin_t9

  , pin_t13 :: Pin_t13
  , pin_t14 :: Pin_t14
  , pin_t15 :: Pin_t15
  , pin_t16 :: Pin_t16
  , pin_t17 :: Pin_t17
  , pin_t18 :: Pin_t18
  , pin_t19 :: Pin_t19
  , pin_t20 :: Pin_t20
  , pin_t21 :: Pin_t21
  }
