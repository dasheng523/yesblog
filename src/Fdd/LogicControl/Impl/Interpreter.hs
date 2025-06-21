module Fdd.LogicControl.Impl.Interpreter where

import qualified Fdd.LogicControl.Language as L

import qualified Fdd.Hardware.Impl.Interpreters.DeviceControl as DCImpl
import qualified Fdd.Hardware.Impl.Interpreters.Hdl as HdlImpl
import qualified Fdd.Hardware.Impl.Runtime as RImpl

import Control.Monad.Free (foldFree)

interpretLogicControlMethod ::
    RImpl.HardwareRuntime ->
    L.LogicControlMethod a ->
    IO a
interpretLogicControlMethod
    hardwareRuntime
    (L.EvalHdl hdl next) = do
        res <- HdlImpl.runHdl hardwareRuntime hdl
        pure $ next res
interpretLogicControlMethod
    hardwareRuntime
    (L.EvalDeviceControlMethod dc next) = do
        res <- DCImpl.interpretDeviceControlMethod hardwareRuntime dc
        pure $ next res
interpretLogicControlMethod
    _
    (L.Report _ next) = do
        pure $ next ()
interpretLogicControlMethod
    _
    (L.Store _ _ next) = do
        pure $ next ()
interpretLogicControlMethod
    _
    (L.Load _ _) = error "Load not implemented"

runLogicControl ::
    RImpl.HardwareRuntime ->
    L.LogicControl a ->
    IO a
runLogicControl hardwareRuntime (L.LogicControl lControl) =
    foldFree (interpretLogicControlMethod hardwareRuntime) lControl