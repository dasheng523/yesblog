module Fdd.Common.Property where

import Fdd.Common.Physics
import Fdd.Common.Value

type PropertyName = String

data Property
    = ValueProperty PropertyName Value
    | PhysicalUnitProperty PropertyName PhysicalUnit
    deriving stock (Show, Eq, Ord)