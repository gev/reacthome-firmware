module Device.Board where

import           Device.GPIO
import           Device.Pin


newtype Board = Board [Pin GPIO]


bottom :: Board
bottom = Board
  [ Pin 1   [ IO  1 IN  ,IO  1 OUT  ,UART 1 TX    ]    ,Pin 11   [ IO  2 IN  ,IO  2 OUT  ,UART 1 RE_DE ]
  , Pin 2   [ IO  3 IN  ,IO  3 OUT  ,UART 2 RE_DE ]    ,Pin 12   [ IO  4 IN  ,IO  4 OUT  ,UART 1 RX    ]
  , Pin 3   [ IO  5 IN  ,IO  5 OUT  ,UART 2 RX    ]    ,Pin 13   [ IO  6 IN  ,IO  6 OUT  ,UART 2 TX    ]
  , Pin 4   [ IO  7 IN  ,IO  7 OUT  ,UART 3 TX    ]    ,Pin 14   [ IO  8 IN  ,IO  8 OUT  ,UART 3 RE_DE ]
  , Pin 5   [ IO  9 IN  ,IO  9 OUT  ,UART 4 TX    ]    ,Pin 15   [ IO 10 IN  ,IO 10 OUT  ,UART 3 RX    ]
  , Pin 6   [ IO 11 IN  ,IO 11 OUT  ,UART 4 RE_DE ]    ,Pin 16   [ IO 12 IN  ,IO 12 OUT  ,UART 4 RX    ]
  , Pin 7   [ IO 13 IN  ,IO 13 OUT  ,I2C  1 SDA   ]    ,Pin 17   [ IO 14 IN  ,IO 14 OUT  ,I2C  1 SCL   ]
  ]

top :: Board
top = Board
  [ Pin 1   [ IO 15 IN  ,IO 15 OUT  ,UART 5 TX    ]    ,Pin 13   [ IO 16 IN  ,IO 16 OUT  ,UART 5 RX    ]
  , Pin 2   [ IO 17 IN  ,IO 17 OUT  ,UART 6 RE_DE ]    ,Pin 14   [ IO 18 IN  ,IO 18 OUT  ,UART 5 RE_DE ]
  , Pin 3   [ IO 19 IN  ,IO 19 OUT  ,UART 6 TX    ]    ,Pin 15   [ IO 20 IN  ,IO 20 OUT  ,UART 6 RX    ]
  , Pin 4   [ IO 21 IN  ,IO 21 OUT  ,I2C  2 SCL   ]    ,Pin 16   [ IO 22 IN  ,IO 22 OUT  ,I2C  2 SDA   ]
  , Pin 5   [ IO 23 IN  ,IO 23 OUT  ,SPI  1 SCKL  ]    ,Pin 17   [ IO 24 IN  ,IO 24 OUT  ,SPI  1 MISO  ]
  , Pin 6   [ IO 25 IN  ,IO 25 OUT  ,SPI  1 MOSI  ]    ,Pin 18   [ IO 26 IN  ,IO 26 OUT  ,SPI  1 SS    ]
  , Pin 7   [ IO 27 IN  ,IO 27 OUT                ]    ,Pin 19   [ IO 28 IN  ,IO 28 OUT                ]
  , Pin 8   [ IO 29 IN  ,IO 29 OUT                ]    ,Pin 20   [ IO 30 IN  ,IO 30 OUT                ]
  , Pin 9   [ IO 30 IN  ,IO 30 OUT                ]    ,Pin 21   [ IO 32 IN  ,IO 32 OUT                ]
  ]
