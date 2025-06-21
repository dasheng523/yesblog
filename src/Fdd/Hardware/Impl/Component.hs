module Fdd.Hardware.Impl.Component where

import Fdd.Common.Physics
import Fdd.Hardware.Common

data VendorComponent
    = VendoredSensor ComponentPassport SensorAPI
    | VendoredController ComponentPassport ControllerAPI

type VendorComponents = Map ComponentName VendorComponent

data SensorAPI = SensorAPI
    { reset :: IO ()
    , readMeasurement :: IO SensorMeasurement
    , setCallback :: Period -> IO SensorMeasurement -> IO ()
    }

data ControllerAPI = ControllerAPI
    { reboot :: IO ()
    , turnOff :: IO ()
    , eval :: String -> IO ()
    , doSomethingElse :: IO ()
    }