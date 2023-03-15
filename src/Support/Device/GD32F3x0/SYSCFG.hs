{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.SYSCFG
    ( EXTI_PORT(..)
    , EXTI_PIN(..)
    , configExtiLine
    , inclSYSCFG
    ) where

import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Support
import           Ivory.Support.Device.GD32F3x0

data EXTI_PORT
    = EXTI_SOURCE_GPIOA
    | EXTI_SOURCE_GPIOB
    | EXTI_SOURCE_GPIOC
    | EXTI_SOURCE_GPIOD
    | EXTI_SOURCE_GPIOF
    deriving (Show, Enum, Bounded)
instance ExtDef EXTI_PORT Uint8


data EXTI_PIN
    = EXTI_SOURCE_PIN0
    | EXTI_SOURCE_PIN1
    | EXTI_SOURCE_PIN2
    | EXTI_SOURCE_PIN3
    | EXTI_SOURCE_PIN4
    | EXTI_SOURCE_PIN5
    | EXTI_SOURCE_PIN6
    | EXTI_SOURCE_PIN7
    | EXTI_SOURCE_PIN8
    | EXTI_SOURCE_PIN9
    | EXTI_SOURCE_PIN10
    | EXTI_SOURCE_PIN11
    | EXTI_SOURCE_PIN12
    | EXTI_SOURCE_PIN13
    | EXTI_SOURCE_PIN14
    | EXTI_SOURCE_PIN15
    deriving (Show, Enum, Bounded)
instance ExtDef EXTI_PIN Uint8


inclSYSCFG :: ModuleM ()
inclSYSCFG = do
    inclDef (def :: Cast EXTI_PORT Uint8)
    inclDef (def :: Cast EXTI_PIN  Uint8)
    incl syscfg_exti_line_config


configExtiLine :: EXTI_PORT -> EXTI_PIN -> Ivory eff ()
configExtiLine port pin = call_ syscfg_exti_line_config (def port) (def pin)

syscfg_exti_line_config :: Def ('[Uint8, Uint8] :-> ())
syscfg_exti_line_config = fun "syscfg_exti_line_config"
