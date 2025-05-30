module Fdd.Hardware.Common where

-- 每个组件的GUID
type PhysicalGuid = String

-- 组件按名称分组
type ComponentName = String

data Parameter = Temperature | Pressure
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

data ComponentClass = Sensors Parameter | Controllers
    deriving stock (Show, Eq, Ord)

data Measurement = Measurement Parameter Float
    deriving stock (Show, Eq, Ord)

data Period = Secondly
    deriving stock (Show, Eq, Ord)