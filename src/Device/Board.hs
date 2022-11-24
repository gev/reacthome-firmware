module Device.Board where

import           Device.Board.Pin

data Board
  = Main    { pin_m1 :: Pin_m1
            }

  | Bottom  { pin_b1  :: Pin_b1
            , pin_b2  :: Pin_b2
            , pin_b3  :: Pin_b3
            , pin_b4  :: Pin_b4
            , pin_b5  :: Pin_b5
            , pin_b6  :: Pin_b6
            , pin_b7  :: Pin_b7

            , pin_b11 :: Pin_b11
            , pin_b12 :: Pin_b12
            , pin_b13 :: Pin_b13
            , pin_b14 :: Pin_b14
            , pin_b15 :: Pin_b15
            , pin_b16 :: Pin_b16
            , pin_b17 :: Pin_b17
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
