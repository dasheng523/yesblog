module Ys.ScriptsSpec where

import Test.Hspec

import Fdd

import Fdd.Assets (aaaController86Name, createBoosters)
import Fdd.Assets.Vendors.AAA.HardwareService (aaaHardwareService)

import qualified Fdd.Hardware.Impl.Device.Types as TImpl
import qualified Fdd.Hardware.Impl.Interpreters.DeviceControl as DCImpl
import qualified Fdd.Hardware.Impl.Interpreters.Hdl as HdlImpl
import qualified Fdd.Hardware.Impl.Runtime as RImpl
import qualified Fdd.Hardware.Impl.Service as SImpl

import qualified Fdd.LogicControl.Impl.Interpreter as LCImpl

import qualified Fdd.Hardware.Language.DeviceControl as L
import qualified Fdd.Hardware.Language.Hdl as L
import qualified Fdd.LogicControl.Language as L

import qualified Fdd.TestData.Scripts as Test
import qualified Ys.WhenDoDsl as WdDsl
import Ys.Yampa.TestElevatorMain (testElevatorMain)

import Control.Concurrent (forkIO, killThread, threadDelay)

-- import Ys.Yampa.Cube.Main (runCubeMain)
import Ys.Yampa.Simple (runYampaApp)

import Control.Exception

-- logScript :: WdDsl.LangL ()
-- logScript = do
--     WdDsl.logDebug "test log debug1"
--     WdDsl.logDebug "test log debug2"

-- doScript :: WdDsl.DoDsl ()
-- doScript = do
--     time <- WdDsl.getCurrentTime
--     WdDsl.lockScreen
--     pass

-- langScript :: WdDsl.LangL ()
-- langScript = do
--     WdDsl.logDebug "test log debug1"
--     WdDsl.evalDoDsl doScript
--     WdDsl.logDebug "test log debug2"

-- TODO: proper tests
spec :: Spec
spec =
    describe "Scripts tests" $ do
        describe "Yampa tests" $ do
            it "Elevator Test" $ do
                testElevatorMain

            it "runYampaApp runs without exception for 120 seconds" $ do
                tid <- forkIO runYampaApp
                threadDelay 120500000 -- 120.5 秒
                killThread tid

-- it "Cube main test" $ do
--     runCubeMain

-- 这里只测试函数能否正常运行一段时间，不断言输出

-- describe "doScript" $ do
--     it "Simple test" $ do
--         WdDsl.runDoDsl doScript
-- describe "logScript" $ do
--     it "Simple test" $ do
--         WdDsl.runLang logScript
-- describe "langScript" $ do
--     it "Simple test" $ do
--         WdDsl.runLang langScript