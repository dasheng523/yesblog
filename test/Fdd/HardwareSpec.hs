module Fdd.HardwareSpec where

import Test.Hspec

import Fdd

import Fdd.Assets (aaaController86Name)
import Fdd.Assets.Vendors.AAA.HardwareService (aaaHardwareService)
import Fdd.TestData.Components (pressure1Passp, thermometer1Passp)
import qualified Fdd.TestData.Scripts as Test

import qualified Fdd.Hardware.Impl.Device.Types as TImpl
import qualified Fdd.Hardware.Impl.Interpreters.DeviceControl as DCImpl
import qualified Fdd.Hardware.Impl.Interpreters.Hdl as HdlImpl
import qualified Fdd.Hardware.Impl.Runtime as RImpl
import qualified Fdd.Hardware.Impl.Service as SImpl

import qualified Fdd.Hardware.Language.Hdl as L

import qualified Data.Map as Map
import Fdd.Hardware.Impl.Runtime

verifyTemperature :: Float -> SensorAPI -> IO ()
verifyTemperature temp handler = do
    measurement <- readMeasurement handler
    measurement `shouldBe` SensorMeasurement (UnitTemperature $ Kelvin temp)

getDevice :: RImpl.HardwareRuntime -> Controller -> IO (TImpl.ControllerImpl, TImpl.Device)
getDevice RImpl.HardwareRuntime{_devicesRef} ctrl = do
    devices <- readIORef _devicesRef
    case Map.lookup ctrl devices of
        Nothing -> fail "Controller not found"
        Just deviceImpl -> pure deviceImpl

getDevicePart' ::
    RImpl.HardwareRuntime ->
    ComponentIndex ->
    Controller ->
    IO (Maybe TImpl.DevicePart)
getDevicePart' runtime idx ctrl = do
    let RImpl.HardwareRuntime{_devicesRef, _hardwareServiceRef} = runtime
    service <- readIORef _hardwareServiceRef
    (_, device) <- getDevice runtime ctrl
    SImpl.getDevicePart service idx device

spec :: Spec
spec =
    describe "Hardware tests" $ do
        it "Hardware device components check" $ do
            runtime <- RImpl.createHardwareRuntime aaaHardwareService

            (leftBoosterCtrl, rightBoosterCtrl) <- HdlImpl.runHdl runtime Test.createBoosters

            mbThermometer1 <- getDevicePart' runtime nozzle1t leftBoosterCtrl
            mbThermometer2 <- getDevicePart' runtime nozzle2t rightBoosterCtrl

            mbNonExistentTherm1 <- getDevicePart' runtime (ComponentIndex "xxx-t") leftBoosterCtrl
            mbNonExistentTherm2 <- getDevicePart' runtime (ComponentIndex "xxx-t") rightBoosterCtrl

            case (mbNonExistentTherm1, mbNonExistentTherm2) of
                (Nothing, Nothing) -> pass
                _ -> fail "Found an unexpected thermometer"

            case (mbThermometer1, mbThermometer2) of
                (Just therm1, Just therm2) -> putStrLn "Component found."
                _ -> fail "There is no such component"

        it "Hardware device component method run" $ do
            runtime <- RImpl.createHardwareRuntime aaaHardwareService

            (leftBoosterCtrl, _) <- HdlImpl.runHdl runtime Test.createBoosters
            mbThermometer <- getDevicePart' runtime nozzle1t leftBoosterCtrl

            case mbThermometer of
                Nothing -> fail "There is no such component"
                Just thermometer -> TImpl.withHandler thermometer (verifyTemperature 3000.0)