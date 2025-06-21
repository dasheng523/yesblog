module Fdd.TestData.Scripts where

import Fdd

import Fdd.Assets (aaaController86Name, createBoosters)
import Fdd.Assets.Vendors.AAA.HardwareService (aaaHardwareService)
import qualified Fdd.Hardware.Language.DeviceControl as L
import qualified Fdd.Hardware.Language.Hdl as L
import qualified Fdd.LogicControl.Language as L

import qualified Data.Map as Map

createBoosters :: Hdl (Controller, Controller)
createBoosters = do
    lCtrl <- L.setupController lBooster lBoosterController aaaController86Passport
    L.registerComponent lCtrl nozzle1p aaaPressure02Passport
    L.registerComponent lCtrl nozzle1t aaaTemperature25Passport

    rCtrl <- L.setupController rBooster rBoosterController aaaController86Passport
    L.registerComponent rCtrl nozzle2p aaaPressure02Passport
    L.registerComponent rCtrl nozzle2t aaaTemperature25Passport
    pure (lCtrl, rCtrl)

getControllerStatus :: Controller -> LogicControl (Either LogicControlFailure ControllerStatus)
getControllerStatus = L.getStatus

createRotaryThruster :: Hdl Controller
createRotaryThruster = do
    ctrl <-
        L.setupController
            rotaryThruster
            rotaryThrusterController
            aaaController86Passport
    L.registerComponent ctrl nozzle1p aaaPressure02Passport
    L.registerComponent ctrl nozzle1t aaaTemperature25Passport
    pure ctrl

createMainEngine :: Hdl Controller
createMainEngine = do
    ctrl <-
        L.setupController
            mainEngine
            mainEngineController
            aaaController86Passport
    L.registerComponent ctrl nozzle1p aaaPressure02Passport
    L.registerComponent ctrl nozzle1t aaaTemperature25Passport
    pure ctrl