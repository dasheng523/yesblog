module Fdd.Assets.Vendors.AAA.HardwareService (
    aaaHardwareService,
) where

import Fdd.Assets.Vendors.AAA.ComponentsAPI (aaaVendorComponents)
import Fdd.Hardware (HardwareService (..))
import qualified Fdd.Hardware.Impl.Device as D

aaaHardwareService :: HardwareService
aaaHardwareService =
    HardwareService
        { makeDevice = pure . D.makeDevice aaaVendorComponents
        , getBlankDevice = pure D.blankDevice
        , getDevicePart = \idx device -> pure (D.getDevicePart idx device)
        }