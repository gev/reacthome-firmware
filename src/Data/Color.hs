{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Color where

import           Ivory.Language
import           Ivory.Stdlib
import           Support.Cast


type RGB = "RGB"

[ivory|
    struct RGB
    { r :: IFloat
    ; g :: IFloat
    ; b :: IFloat
    }
|]

rgb :: IFloat -> IFloat -> IFloat -> [InitStruct RGB]
rgb r' g' b' =
    [ r .= ival r'
    , g .= ival g'
    , b .= ival b'
    ]


type HSV = "HSV"

[ivory|
    struct HSV
    { h :: IFloat
    ; s :: IFloat
    ; v :: IFloat
    }
|]

hsv :: IFloat -> IFloat -> IFloat -> [InitStruct HSV]
hsv h' s' v' =
    [ h .= ival h'
    , s .= ival s'
    , v .= ival v'
    ]


hsv'to'rgb :: Ref s1 (Struct HSV)
           -> Ref s2 (Struct RGB)
           -> Ivory eff ()
hsv'to'rgb hsv rgb = do
    h'        <- castFloatToUint16 =<< deref (hsv ~> h)
    s'        <- deref (hsv ~> s)
    v'        <- deref (hsv ~> v)
    let h'i    = (h' `iDiv` 60) .% 6
    let v'min  = (1 - s') * v'
    let a'     = (v' - v'min) * safeCast (h' .% 60) / 60
    let v'inc  = v'min + a'
    let v'dec  = v'    - a'
    cond_ [ h'i ==? 0 ==> (do store (rgb ~> r) v'
                              store (rgb ~> g) v'inc
                              store (rgb ~> b) v'min
                          )
          , h'i ==? 1 ==> (do store (rgb ~> r) v'dec
                              store (rgb ~> g) v'
                              store (rgb ~> b) v'min
                          )
          , h'i ==? 2 ==> (do store (rgb ~> r) v'min
                              store (rgb ~> g) v'
                              store (rgb ~> b) v'inc
                          )
          , h'i ==? 3 ==> (do store (rgb ~> r) v'min
                              store (rgb ~> g) v'dec
                              store (rgb ~> b) v'
                          )
          , h'i ==? 4 ==> (do store (rgb ~> r) v'inc
                              store (rgb ~> g) v'min
                              store (rgb ~> b) v'
                          )
          , h'i ==? 5 ==> (do store (rgb ~> r) v'
                              store (rgb ~> g) v'min
                              store (rgb ~> b) v'dec
                          )
          ]
