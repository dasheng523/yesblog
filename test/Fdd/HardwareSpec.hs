module Fdd.HardwareSpec where

import Fdd
import Fdd.Assets.DeviceDefinitions (boostersDef)
import Fdd.Assets.Vendors.AAA.HardwareService (aaaHardwareService)
import Fdd.Test.HardwareService
import Fdd.TestData.Components
import Test.Hspec

verifyTemperature :: Float -> SensorAPI -> IO ()
verifyTemperature temp handler = do
    measurement <- readMeasurement handler
    measurement `shouldBe` Measurement Temperature temp

spec :: Spec
spec =
    describe "Hardware tests" $ do
        it "Hardware device components check" $ do
            boosters <- makeDevice aaaHardwareService boostersDef
            mbThermometer <- getDevicePart aaaHardwareService "nozzle1-t" boosters

            case mbThermometer of
                Nothing -> fail "There is no such component"
                Just thermometer -> putStrLn "Component found."

        it "Hardware device component method run" $ do
            boosters <- makeDevice aaaHardwareService boostersDef
            mbThermometer <- getDevicePart aaaHardwareService "nozzle1-t" boosters
            case mbThermometer of
                Nothing -> fail "There is no such component"
                Just thermometer -> withHandler thermometer (verifyTemperature 100.0)

        it "Getting measurement from mocked device" $ do
            let testDef =
                    [ComponentDef "t1" thermometer1Passp]
            device <- makeDevice mockedHardwareService testDef
            mpPart <- getDevicePart mockedHardwareService "t1" device
            case mpPart of
                Nothing -> fail "There is no such part"
                Just part -> withHandler part (verifyTemperature 50.0)

        it "Getting absent device part" $ do
            let testDef =
                    [ ComponentDef "t1" thermometer1Passp
                    , ComponentDef "p1" pressure1Passp
                    ]
            device <- makeDevice mockedHardwareService testDef
            mpPart1 <- getDevicePart mockedHardwareService "t1" device
            mpPart2 <- getDevicePart mockedHardwareService "p1" device
            mpPart3 <- getDevicePart mockedHardwareService "t2" device

            case (mpPart1, mpPart2, mpPart3) of
                (Just _, Just _, Nothing) -> pass
                _ -> fail "Device is assembled incorrectly."
