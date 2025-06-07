module Fdd.Assets.Vendors.AAA.HardwareService (
    aaaHardwareService,
) where

import Fdd.Assets.Vendors.AAA.ComponentsAPI (aaaVendorComponents)
import Fdd.Hardware (HardwareService (..))
import qualified Fdd.Hardware.Impl.Device as Impl

aaaHardwareService :: HardwareService
aaaHardwareService =
    HardwareService
        { makeController = Impl.makeController aaaVendorComponents
        , makeDevicePart = Impl.makeDevicePart aaaVendorComponents
        , makeBlankDevice = Impl.makeBlankDevice
        , addDevicePart = Impl.addDevicePart
        , getDevicePart = Impl.getDevicePart
        }