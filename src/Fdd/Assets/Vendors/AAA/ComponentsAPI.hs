module Fdd.Assets.Vendors.AAA.ComponentsAPI where

import Fdd.Assets.Vendors.AAA.Components
import Fdd.Hardware.Common
import Fdd.Hardware.Impl.Component

aaaTemperature25Handler :: SensorAPI
aaaTemperature25Handler =
  SensorAPI
    { -- 不执行任何操作：
      reset = putStrLn $ aaaTemperature25Name <> " reset."
    , -- 返回一个虚拟温度：
      readMeasurement = pure $ Measurement Temperature 100.0
    , -- 不执行任何操作：
      setCallback = \_ _ -> putStrLn $ aaaTemperature25Name <> " callback"
    }

aaaPressure02Handler :: SensorAPI
aaaPressure02Handler =
  SensorAPI
    { reset = putStrLn $ aaaPressure02Name <> " reset."
    , readMeasurement = pure $ Measurement Pressure 100.0 -- dummy
    , setCallback = \_ _ -> putStrLn $ aaaPressure02Name <> " callback."
    }

aaaController86Handler :: ControllerAPI
aaaController86Handler =
  ControllerAPI
    { reboot = putStrLn $ aaaController86Name <> " reset."
    , turnOff = putStrLn $ aaaController86Name <> " turn off."
    , eval = \cmd -> putStrLn $ aaaController86Name <> " eval cmd: " <> cmd
    , doSomethingElse = putStrLn $ aaaController86Name <> " do something else"
    }

aaaVendorComponents :: VendorComponents
aaaVendorComponents =
  fromList
    [ (aaaTemperature25Name, VendoredSensor aaaTemperature25Passport aaaTemperature25Handler)
    , (aaaPressure02Name, VendoredSensor aaaPressure02Passport aaaPressure02Handler)
    , (aaaController86Name, VendoredController aaaController86Passport aaaController86Handler)
    ]