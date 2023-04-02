{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.SYSCFG
    ( EXTI_PORT
    , EXTI_PIN

    , exti_source_gpioa
    , exti_source_gpiob
    , exti_source_gpioc
    , exti_source_gpiod
    , exti_source_gpiof

    , exti_source_pin0
    , exti_source_pin1
    , exti_source_pin2
    , exti_source_pin3
    , exti_source_pin4
    , exti_source_pin5
    , exti_source_pin6
    , exti_source_pin7
    , exti_source_pin8
    , exti_source_pin9
    , exti_source_pin10
    , exti_source_pin11
    , exti_source_pin12
    , exti_source_pin13
    , exti_source_pin14
    , exti_source_pin15

    , configExtiLine

    , inclSYSCFG
    ) where

import           Ivory.Language
import           Ivory.Support.Device.GD32F3x0



newtype EXTI_PORT = EXTI_PORT Uint8
    deriving (IvoryExpr, IvoryInit, IvoryVar, IvoryType)

exti_source_gpioa = EXTI_PORT $ ext "EXTI_SOURCE_GPIOA"
exti_source_gpiob = EXTI_PORT $ ext "EXTI_SOURCE_GPIOB"
exti_source_gpioc = EXTI_PORT $ ext "EXTI_SOURCE_GPIOC"
exti_source_gpiod = EXTI_PORT $ ext "EXTI_SOURCE_GPIOD"
exti_source_gpiof = EXTI_PORT $ ext "EXTI_SOURCE_GPIOF"



newtype EXTI_PIN = EXTI_PIN Uint8
    deriving (IvoryExpr, IvoryInit, IvoryVar, IvoryType)

exti_source_pin0  = EXTI_PIN $ ext "EXTI_SOURCE_PIN0"
exti_source_pin1  = EXTI_PIN $ ext "EXTI_SOURCE_PIN1"
exti_source_pin2  = EXTI_PIN $ ext "EXTI_SOURCE_PIN2"
exti_source_pin3  = EXTI_PIN $ ext "EXTI_SOURCE_PIN3"
exti_source_pin4  = EXTI_PIN $ ext "EXTI_SOURCE_PIN4"
exti_source_pin5  = EXTI_PIN $ ext "EXTI_SOURCE_PIN5"
exti_source_pin6  = EXTI_PIN $ ext "EXTI_SOURCE_PIN6"
exti_source_pin7  = EXTI_PIN $ ext "EXTI_SOURCE_PIN7"
exti_source_pin8  = EXTI_PIN $ ext "EXTI_SOURCE_PIN8"
exti_source_pin9  = EXTI_PIN $ ext "EXTI_SOURCE_PIN9"
exti_source_pin10 = EXTI_PIN $ ext "EXTI_SOURCE_PIN10"
exti_source_pin11 = EXTI_PIN $ ext "EXTI_SOURCE_PIN11"
exti_source_pin12 = EXTI_PIN $ ext "EXTI_SOURCE_PIN12"
exti_source_pin13 = EXTI_PIN $ ext "EXTI_SOURCE_PIN13"
exti_source_pin14 = EXTI_PIN $ ext "EXTI_SOURCE_PIN14"
exti_source_pin15 = EXTI_PIN $ ext "EXTI_SOURCE_PIN15"



configExtiLine :: EXTI_PORT -> EXTI_PIN -> Ivory eff ()
configExtiLine = call_ syscfg_exti_line_config

syscfg_exti_line_config :: Def ('[EXTI_PORT, EXTI_PIN] :-> ())
syscfg_exti_line_config = fun "syscfg_exti_line_config"



inclSYSCFG :: ModuleDef
inclSYSCFG = do
    inclSym exti_source_gpioa
    inclSym exti_source_gpiob
    inclSym exti_source_gpioc
    inclSym exti_source_gpiod
    inclSym exti_source_gpiof

    inclSym exti_source_pin0
    inclSym exti_source_pin1
    inclSym exti_source_pin2
    inclSym exti_source_pin3
    inclSym exti_source_pin4
    inclSym exti_source_pin5
    inclSym exti_source_pin6
    inclSym exti_source_pin7
    inclSym exti_source_pin8
    inclSym exti_source_pin9
    inclSym exti_source_pin10
    inclSym exti_source_pin11
    inclSym exti_source_pin12
    inclSym exti_source_pin13
    inclSym exti_source_pin14
    inclSym exti_source_pin15

    incl syscfg_exti_line_config
