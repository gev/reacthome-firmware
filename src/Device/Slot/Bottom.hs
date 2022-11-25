module Device.Slot.Bottom where


data Pin_1  = IO_1_IN  | IO_1_OUT  | UART_1_TX
data Pin_2  = IO_3_IN  | IO_3_OUT  | UART_2_RE_DE
data Pin_3  = IO_5_IN  | IO_5_OUT  | UART_2_RX
data Pin_4  = IO_7_IN  | IO_7_OUT  | UART_3_TX
data Pin_5  = IO_9_IN  | IO_9_OUT  | UART_4_TX
data Pin_6  = IO_11_IN | IO_11_OUT | UART_4_RE_DE
data Pin_7  = IO_13_IN | IO_13_OUT | I2C_1_SDA

data Pin_11 = IO_2_IN  | IO_2_OUT  | UART_1_RE_DE
data Pin_12 = IO_4_IN  | IO_4_OUT  | UART_1_RX
data Pin_13 = IO_6_IN  | IO_6_OUT  | UART_2_TX
data Pin_14 = IO_8_IN  | IO_8_OUT  | UART_3_RE_DE
data Pin_15 = IO_10_IN | IO_10_OUT | UART_3_RX
data Pin_16 = IO_12_IN | IO_12_OUT | UART_4_RX
data Pin_17 = IO_14_IN | IO_14_OUT | I2C_1_SCL


data Slot = Slot  
  { pin_1  :: Pin_1
  , pin_2  :: Pin_2
  , pin_3  :: Pin_3
  , pin_4  :: Pin_4
  , pin_5  :: Pin_5
  , pin_6  :: Pin_6
  , pin_7  :: Pin_7

  , pin_11 :: Pin_11
  , pin_12 :: Pin_12
  , pin_13 :: Pin_13
  , pin_14 :: Pin_14
  , pin_15 :: Pin_15
  , pin_16 :: Pin_16
  , pin_17 :: Pin_17
  }

  | Top     { pin_t1  :: Pin_t1
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
