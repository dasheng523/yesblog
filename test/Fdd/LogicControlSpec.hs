module Fdd.LogicControlSpec where

import Fdd
import Test.Hspec

import Fdd.Assets (aaaController86Name, boostersDef)
import Fdd.Assets.Vendors.AAA.HardwareService (aaaHardwareService)
import Fdd.TestData.Components (pressure1Passp, thermometer1Passp)

import qualified Fdd.Hardware.Impl.Device.Types as TImpl
import qualified Fdd.Hardware.Impl.Interpreters.Hdl as HdlImpl
import qualified Fdd.Hardware.Impl.Runtime as RImpl
import qualified Fdd.Hardware.Impl.Service as SImpl
import qualified Fdd.Hardware.Language.Hdl as L

import qualified Fdd.LogicControl.Impl.Interpreters.LogicControl as LCImpl

import qualified Data.Map as Map

verifyTemperature :: Float -> SensorAPI -> IO ()
verifyTemperature temp handler = do
    measurement <- readMeasurement handler
    measurement `shouldBe` Measurement Temperature temp

getDevice :: RImpl.Runtime -> ControlerName -> IO TImpl.Device
getDevice runtime ctrlName = do
    let devices = RImpl._devices runtime
    case Map.lookup (Controller ctrlName) devices of
        Nothing -> fail "Controller not found"
        Just (_, device) -> pure device

spec :: Spec
spec =
    describe "Hardware tests" $ do
        it "Hardware device components check" $ do
            (putStrLn "Test" :: IO ())