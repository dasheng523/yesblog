module Fdd.Hardware.Impl.Component where

import Fdd.Hardware.Common

data VendorComponent
    = VendoredSensor ComponentPassport SensorAPI
    | VendoredController ComponentPassport ControllerAPI

type VendorComponents = Map ComponentName VendorComponent

data SensorAPI = SensorAPI
    { reset :: IO ()
    , readMeasurement :: IO Measurement
    , setCallback :: Period -> IO Measurement -> IO ()
    }

data ControllerAPI = ControllerAPI
    { reboot :: IO ()
    , turnOff :: IO ()
    , eval :: String -> IO ()
    , doSomethingElse :: IO ()
    }