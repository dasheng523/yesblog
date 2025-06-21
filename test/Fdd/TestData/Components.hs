module Fdd.TestData.Components where

import Fdd

thermometer1Passp :: ComponentPassport
thermometer1Passp =
    ComponentPassport (Sensors TemperatureSensor) "t1" "t1" "t1"

pressure1Passp :: ComponentPassport
pressure1Passp =
    ComponentPassport (Sensors PressureSensor) "p1" "p1" "p1"

controller1Passp :: ComponentPassport
controller1Passp =
    ComponentPassport Controllers "controller" "controller" "controller"

thermometer1Handler :: SensorAPI
thermometer1Handler =
    SensorAPI
        { reset = putStrLn "t1 reset."
        , readMeasurement = pure $ SensorMeasurement $ UnitTemperature $ Kelvin 50.0
        , setCallback = \_ _ -> putStrLn "t1 callback."
        }

pressure1Handler :: SensorAPI
pressure1Handler =
    SensorAPI
        { reset = putStrLn "p1 reset."
        , readMeasurement = pure (SensorMeasurement $ UnitPressure $ Pascal 2.0)
        , setCallback = \_ _ -> putStrLn "p1 callback."
        }