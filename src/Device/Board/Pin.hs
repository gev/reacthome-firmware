module Device.Board.Pin where

data Pin_m1  = LED_1
data Pin_m2  = LED_2


data Pin_b1  = IO_1_IN  | IO_1_OUT  | UART_1_TX
data Pin_b2  = IO_3_IN  | IO_3_OUT  | UART_2_RE_DE
data Pin_b3  = IO_5_IN  | IO_5_OUT  | UART_2_RX
data Pin_b4  = IO_7_IN  | IO_7_OUT  | UART_3_TX
data Pin_b5  = IO_9_IN  | IO_9_OUT  | UART_4_TX
data Pin_b6  = IO_11_IN | IO_11_OUT | UART_4_RE_DE
data Pin_b7  = IO_13_IN | IO_13_OUT | I2C_1_SDA


data Pin_b11 = IO_2_IN  | IO_2_OUT  | UART_1_RE_DE
data Pin_b12 = IO_4_IN  | IO_4_OUT  | UART_1_RX
data Pin_b13 = IO_6_IN  | IO_6_OUT  | UART_2_TX
data Pin_b14 = IO_8_IN  | IO_8_OUT  | UART_3_RE_DE
data Pin_b15 = IO_10_IN | IO_10_OUT | UART_3_RX
data Pin_b16 = IO_12_IN | IO_12_OUT | UART_4_RX
data Pin_b17 = IO_14_IN | IO_14_OUT | I2C_1_SCL


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
