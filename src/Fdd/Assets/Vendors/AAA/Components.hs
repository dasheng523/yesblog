module Fdd.Assets.Vendors.AAA.Components where

import Fdd.Assets.Vendors.AAA.Common
import Fdd.Hardware.Common

aaaTemperature25Name :: ComponentName
aaaTemperature25Name = "AAA-T-25"

aaaPressure02Name :: ComponentName
aaaPressure02Name = "AAA-З-02"

aaaController86Name :: ComponentName
aaaController86Name = "AAA-C-86"

guid1 :: PhysicalGuid
guid1 = "some_guid1"
guid2 :: PhysicalGuid
guid2 = "some_guid2"
guid3 :: PhysicalGuid
guid3 = "some_guid3"

aaaTemperature25Passport :: ComponentPassport
aaaTemperature25Passport = ComponentPassport (Sensors TemperatureSensor) aaaTemperature25Name guid2 aaaInc

aaaPressure02Passport :: ComponentPassport
aaaPressure02Passport = ComponentPassport (Sensors PressureSensor) aaaPressure02Name guid1 aaaInc

aaaController86Passport :: ComponentPassport
aaaController86Passport = ComponentPassport Controllers aaaController86Name guid3 aaaInc