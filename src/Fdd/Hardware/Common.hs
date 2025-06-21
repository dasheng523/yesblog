module Fdd.Hardware.Common where

import Fdd.Common.Physics

-- 每个组件的GUID
type PhysicalGuid = String

-- 组件按名称分组
type ComponentName = String

data SensorType
    = TemperatureSensor
    | PressureSensor
    deriving stock (Show, Eq, Ord)

data ComponentClass = Sensors SensorType | Controllers
    deriving stock (Show, Eq, Ord)

newtype SensorMeasurement = SensorMeasurement PhysicalUnit
    deriving stock (Show, Eq, Ord)

-- 供应商
type Vendor = String

data ComponentPassport = ComponentPassport
    { componentClass :: ComponentClass
    , componentName :: ComponentName
    , componentGuid :: PhysicalGuid
    , componentVendor :: Vendor
    }
    deriving stock (Show, Eq, Ord)
