module Fdd.Assets.Scripts where

import Fdd.Assets.Vendors.AAA.Components (aaaController86Passport, aaaTemperature25Passport)
import Fdd.Hardware
import Fdd.Hardware.Domain
import Fdd.Hardware.Language.DeviceControl
import Fdd.LogicControl

script :: LogicControl
script =
    [ EvalHdl
        [ SetupController
            "device"
            "ctrl"
            aaaController86Passport
            ( \ctrl ->
                [ EvalHdl
                    [RegisterComponent ctrl "therm" aaaTemperature25Passport]
                , EvalDeviceControl (readAndReport ctrl)
                ]
            )
        ]
    ]
  where
    readAndReport :: Controller -> DeviceControl LogicControl
    readAndReport ctrl =
        [ ReadSensor
            ctrl
            "therm"
            ( \eMeasurement ->
                [Report (show eMeasurement)]
            )
        ]