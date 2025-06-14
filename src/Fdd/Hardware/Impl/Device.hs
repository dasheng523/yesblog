module Fdd.Hardware.Impl.Device (
    makeBlankDevice,
    makeDevicePart,
    makeController,
    addDevicePart,
    getDevicePart,
) where

import Fdd.Hardware.Common

import qualified Data.Map as Map
import Fdd.Hardware.Domain
import Fdd.Hardware.Impl.Component
import Fdd.Hardware.Impl.Device.Types

makeBlankDevice :: DeviceName -> ControllerImpl -> IO Device
makeBlankDevice name ctrlImpl = do
    partsRef <- newIORef Map.empty
    pure $ Device name ctrlImpl partsRef

makeDevicePart :: VendorComponents -> ComponentPassport -> IO (Either String DevicePart)
makeDevicePart vendorComponents (ComponentPassport (Sensors _) cName _ cVendor) =
    pure $ case Map.lookup cName vendorComponents of
        Just vendorComponent -> Right (DevicePart vendorComponent)
        Nothing -> Left ("Component not found: " <> cVendor <> " " <> cName)
makeDevicePart _ _ = pure $ Left "Invalid/unknown component class for a device part."

makeController :: VendorComponents -> ControllerName -> ComponentPassport -> IO (Either String ControllerImpl)
makeController vendorComponents ctrlName (ComponentPassport Controllers cName _ cVendor) =
    pure $ case Map.lookup cName vendorComponents of
        Just vendorComponent -> Right (ControllerImpl ctrlName vendorComponent)
        Nothing -> Left ("Component not found: " <> cVendor <> " " <> cName)
makeController _ _ _ = pure $ Left "Invalid/unknown component class for a controller."

addDevicePart :: ComponentIndex -> DevicePart -> Device -> IO ()
addDevicePart idx part (Device _ _ partsRef) = do
    parts <- readIORef partsRef
    writeIORef partsRef (Map.insert idx part parts)

getDevicePart :: ComponentIndex -> Device -> IO (Maybe DevicePart)
getDevicePart idx (Device _ _ partsRef) = do
    parts <- readIORef partsRef
    pure $ Map.lookup idx parts
