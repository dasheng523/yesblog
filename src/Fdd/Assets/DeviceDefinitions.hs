module Fdd.Assets.DeviceDefinitions where

import Fdd.Assets.Vendors.AAA.Components
import Fdd.Hardware

boostersDef :: Hdl
boostersDef =
    [ ComponentDef "nozzle1-t" aaaTemperature25Passport
    , ComponentDef "nozzle1-p" aaaPressure02Passport
    , ComponentDef "nozzle2-t" aaaTemperature25Passport
    , ComponentDef "nozzle2-p" aaaPressure02Passport
    , ComponentDef "controller" aaaController86Passport
    ]