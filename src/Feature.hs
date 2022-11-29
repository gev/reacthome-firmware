{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Feature where
import           Ivory.Language
import           Ivory.Language.Module (ModuleDef, ModuleM)

data Dim'
  = AC
  | DC

data RBUS'
  = Master
  | Slave

data AO      a = AO            Int   a
data Buttonn a = Button        Int   a
data ButtonL a = ButtonL Int   Int   a
data Dim     a = Dim     Dim'  Int   a
data Doppler a = Doppler       Int   a
data Eth     a = Eth           Int   a
data In      a = In            Int   a
data Findme  a = Findme        Int   a
data OW      a = OW            Int   a
data Relay   a = Relay         Int   a
data RBUS    a = RBUS    RBUS' Int   a
data RS485   a = RS485         Int   a
data Service a = Service       Int   a

data Feature where
  Feature :: Prepare f => f -> Feature

type Features  = [Feature]

data Pack = Pack
  { dependecies :: [ModuleM ()]
  , initialize  :: Def ('[] :-> ())
  , step        :: Def ('[] :-> ())
  }

class Prepare f where
  prepare :: f -> Pack

instance Prepare Feature where
  prepare (Feature f) = prepare f
