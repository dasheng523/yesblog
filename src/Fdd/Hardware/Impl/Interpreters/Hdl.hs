{-# LANGUAGE OverloadedStrings #-}

module Fdd.Hardware.Impl.Interpreters.Hdl where

import qualified Fdd.Hardware.Language.Hdl as L

import qualified Fdd.Hardware.Domain as T

import Control.Monad (foldM)
import qualified Data.Map as Map
import qualified Fdd.Hardware.Impl.Runtime as RImpl
import qualified Fdd.Hardware.Impl.Service as SImpl

interpretHdlMethod :: RImpl.Runtime -> (RImpl.Runtime -> next -> IO RImpl.Runtime) -> L.HdlMethod next -> IO RImpl.Runtime
interpretHdlMethod runtime nextIterp (L.SetupController deviceName ctrlName passp next) = do
    let devices = RImpl._devices runtime
    let service = RImpl._hardwareService runtime
    eCtrlImpl <- SImpl.makeController service ctrlName passp
    ctrlImpl <- case eCtrlImpl of
        Left err -> error $ toText err
        Right impl -> pure impl
    blankDevice <- SImpl.makeBlankDevice service deviceName ctrlImpl
    let ctrl = T.Controller ctrlName
    let devices' = Map.insert ctrl (ctrlImpl, blankDevice) devices
    let runtime' = RImpl.Runtime devices' service
    nextIterp runtime' $ next ctrl
interpretHdlMethod runtime _ (L.RegisterComponent ctrl idx passp) = do
    let devices = RImpl._devices runtime
    let service = RImpl._hardwareService runtime
    let mbDevice = Map.lookup ctrl devices
    case mbDevice of
        Nothing -> error "Device not found" --- bad practice
        Just (ctrlImpl, device) -> do
            eDevicePart <- SImpl.makeDevicePart service passp
            case eDevicePart of
                Left err -> error $ toText err -- bad practice
                Right part -> do
                    device' <- SImpl.addDevicePart service idx part device
                    let devices' = Map.insert ctrl (ctrlImpl, device') devices
                    let runtime' = RImpl.Runtime devices' service
                    pure runtime'

runHdl :: RImpl.Runtime -> (RImpl.Runtime -> next -> IO RImpl.Runtime) -> L.Hdl next -> IO RImpl.Runtime
runHdl runtime nextInterp =
    foldM (`interpretHdlMethod` nextInterp) runtime