module Fdd.LogicControlSpec where

import Fdd
import Test.Hspec

verifyTemperature :: Float -> SensorAPI -> IO ()
verifyTemperature temp handler = do
    measurement <- readMeasurement handler
    measurement `shouldBe` Measurement Temperature temp

-- getDevice :: RImpl.Runtime -> ControlerName -> IO TImpl.Device
-- getDevice runtime ctrlName = do
--     let devices = RImpl._devices runtime

spec :: Spec
spec =
    describe "Hardware tests" $ do
        it "Hardware device components check" $ do
            (putStrLn "Test" :: IO ())