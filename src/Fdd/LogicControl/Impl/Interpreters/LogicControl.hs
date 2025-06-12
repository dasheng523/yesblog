module Fdd.LogicControl.Impl.Interpreters.LogicControl where

import qualified Fdd.Hardware.Common as T
import qualified Fdd.Hardware.Domain as T
import qualified Fdd.Hardware.Language.DeviceControl as L
import qualified Fdd.Hardware.Language.Hdl as L

import qualified Fdd.Hardware.Impl.Device.Types as TImpl
import qualified Fdd.Hardware.Impl.Runtime as RImpl
import qualified Fdd.Hardware.Impl.Service as SImpl

import qualified Fdd.Hardware.Impl.Interpreters.DeviceControl as DeviceControlImpl
import qualified Fdd.Hardware.Impl.Interpreters.Hdl as HdlImpl

import Control.Monad (foldM)
import qualified Fdd.LogicControl.Domain as T
import qualified Fdd.LogicControl.Language as L

interpreterLogicControlMethod :: RImpl.Runtime -> L.LogicControlMethod -> IO RImpl.Runtime
interpreterLogicControlMethod runtime (L.EvalHdl hdl) =
    HdlImpl.runHdl runtime runLogicControl hdl
interpreterLogicControlMethod runtime (L.EvalDeviceControl dc) =
    DeviceControlImpl.runDeviceControl runtime runLogicControl dc
interpreterLogicControlMethod runtime (L.Report msg) = do
    putStrLn msg
    pure runtime
interpreterLogicControlMethod _ (L.Store _ _) =
    error "not implemented"

runLogicControl :: RImpl.Runtime -> L.LogicControl -> IO RImpl.Runtime
runLogicControl =
    foldM interpreterLogicControlMethod