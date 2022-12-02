{-# LANGUAGE GADTs         #-}

module Feature where
import           Interface.Timer       
import           Ivory.Language
import           Ivory.Language.Module 
import           Prepare

data Dim'
  = AC
  | DC

data AO      a = AO            Int   a
data Button  a = Button        Int   a
data ButtonL a = ButtonL Int   Int   a
data Dim     a = Dim     Dim'  Int   a
data Doppler a = Doppler       Int   a
data Eth     a = Eth           Int   a
data In      a = In            Int   a
data FindMe  a = FindMe        Int   a
data OW      a = OW            Int   a
data RS485   a = RS485         Int   a
data Service a = Service       Int   a

data Feature where
  Feature :: Prepare f => f -> Feature

type Features  = [Feature]

instance Prepare Feature where
  prepare (Feature f) = prepare f
