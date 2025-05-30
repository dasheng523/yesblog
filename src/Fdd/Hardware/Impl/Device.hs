module Fdd.Hardware.Impl.Device (
    Device,
    DevicePart (..),
    WithHandler (..),
    makeDevice,
    blankDevice,
    getDevicePart,
) where

import Fdd.Hardware.Common

import qualified Data.Map.Lazy as Map
import Fdd.Hardware.Impl.Component
import Fdd.Hardware.Language.Hdl

newtype DevicePart = DevicePart VendorComponent
newtype Device = Device (Map ComponentIndex DevicePart)

makeDevice ::
    VendorComponents -> -- 外部状态和真实上下文
    Hdl -> -- 定义
    Device
makeDevice vendorComponents hdl = makeDevice' hdl blankDevice
  where
    makeDevice' [] device = device
    makeDevice' (c : cs) device = makeDevice' cs (add' c device)
    -- 验证组件的定义。
    -- 创建特定的设备部件（实现）
    -- 并添加到 Device 类型中
    add' :: ComponentDef -> Device -> Device
    add' (ComponentDef idx passp) device =
        case validateComponent vendorComponents passp of
            Right part -> addComponent idx part device
            Left err -> error $ toText err -- 不好的实践！

-- 验证函数。在上下文中搜索真实的组件
-- 实现，并生成一个部件
-- 用于设备
validateComponent ::
    VendorComponents -> -- 外部状态和真实上下文
    ComponentPassport -> -- 待验证的组件
    Either String DevicePart
validateComponent
    vendorComponents
    (ComponentPassport _ cName _ _) =
        case Map.lookup cName vendorComponents of
            Nothing -> Left ("Not found: " <> cName)
            Just vendorComponent -> Right (DevicePart vendorComponent)

-- 更新字典
addComponent :: ComponentIndex -> DevicePart -> Device -> Device
addComponent idx part (Device parts) =
    Device (Map.insert idx part parts)

blankDevice :: Device
blankDevice = Device Map.empty

getDevicePart :: ComponentIndex -> Device -> Maybe DevicePart
getDevicePart idx (Device parts) = Map.lookup idx parts

-- 怎么想到这个class呢？
class WithHandler handlerAPI where
    withHandler ::
        DevicePart ->
        (handlerAPI -> IO ()) ->
        IO ()

instance WithHandler SensorAPI where
    withHandler (DevicePart (VendoredSensor _ handler)) f = f handler
    withHandler _ _ = error "Invalid part API handler"

instance WithHandler ControllerAPI where
    withHandler (DevicePart (VendoredController _ handler)) f = f handler
    withHandler _ _ = error "Invalid part API handler"