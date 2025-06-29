module Fdd.ScriptsSpec where

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

import Control.Exception

newtype SpaceshipProperties = SpaceshipProperties
    { spMass :: Mass
    }

data SpaceshipModel = SpaceshipModel
    { smSpaceshipProperties :: SpaceshipProperties
    , smMainEngine :: Controller
    , smRotationThruster :: Controller
    }

type Burn = Command

-- Unsafe function. Yes, I know
validateTorque ::
    Either LogicControlFailure (Maybe Property) ->
    LogicControl Torque
validateTorque (Right (Just (PhysicalUnitProperty _ (UnitTorque (Torque t))))) =
    pure $ Torque t
validateTorque (Left err) = error $ show err

-- Dummy function. Doesn't calculate anything.
calcAngularImpulse ::
    Mass ->
    Torque ->
    LogicControl AngularImpulse
calcAngularImpulse _ _ = pure $ AngularImpulse 0

recalcMass :: Mass -> AngularImpulse -> LogicControl Mass
recalcMass shipMass impulse = pure shipMass -- Dummy

getThrusterAngularImpulse ::
    Controller ->
    Mass ->
    LogicControl AngularImpulse
getThrusterAngularImpulse ctrl mass = do
    eMbTorqueProp <- L.getProperty ctrl "torque" []
    torque <- validateTorque eMbTorqueProp
    calcAngularImpulse mass torque

performBurn :: Controller -> Burn -> LogicControl ()
performBurn _ _ = pass

wait :: Int -> LogicControl ()
wait _ = pass

calcBurn ::
    AngularImpulse ->
    AngularImpulse ->
    Angle ->
    LogicControl (Burn, Burn, Int)
calcBurn _ _ _ = pure (undefined, undefined, 0) -- dummy

performRotation ::
    SpaceshipModel ->
    Angle ->
    LogicControl SpaceshipModel
performRotation model angle = do
    let thrusterCtrl = smRotationThruster model
    let shipMass1 = spMass $ smSpaceshipProperties model

    impulse1 <- getThrusterAngularImpulse thrusterCtrl shipMass1
    shipMass2 <- recalcMass shipMass1 impulse1
    impulse2 <- getThrusterAngularImpulse thrusterCtrl shipMass2
    shipMass3 <- recalcMass shipMass2 impulse2

    (burnStart, burnStop, time) <- calcBurn impulse1 impulse2 angle

    performBurn thrusterCtrl burnStart
    wait time
    performBurn thrusterCtrl burnStop

    pure $ model{smSpaceshipProperties = SpaceshipProperties shipMass3}

spaceshipRotation :: LogicControl (Either String ())
spaceshipRotation = do
    let shipProps = SpaceshipProperties (Kilogram 100000.0)
    mainEngineCtrl <- L.evalHdl Test.createMainEngine
    thrusterCtrl <- L.evalHdl Test.createRotaryThruster
    let shipModel1 = SpaceshipModel shipProps mainEngineCtrl thrusterCtrl
    shipModel2 <- performRotation shipModel1 (Radian 100.0)
    pure $ Right ()

-- TODO: proper tests
spec :: Spec
spec =
    describe "Scripts tests" $ do
        describe "Spaceship rotation script" $ do
            it "Missing vendor components" $ do
                runtime <- RImpl.createHardwareRuntime aaaHardwareService
                eResult <- try $ LCImpl.runLogicControl runtime spaceshipRotation
                case eResult of
                    Left (e :: SomeException) -> pass
                    Right _ -> fail "Unexpected success"