module Fdd.Common.Physics where

data Period = Secondly
    deriving stock (Show, Eq, Ord)

newtype Temperature = Kelvin Float
    deriving stock (Show, Eq, Ord)

newtype Pressure = Pascal Float
    deriving stock (Show, Eq, Ord)

newtype Mass = Kilogram Float
    deriving stock (Show, Eq, Ord)

newtype Velocity = MetersPerSecond Float
    deriving stock (Show, Eq, Ord)

newtype TimeInterval = Seconds Float
    deriving stock (Show, Eq, Ord)

newtype Angle = Radian Float
    deriving stock (Show, Eq, Ord)

newtype Frequency = Hertz Float
    deriving stock (Show, Eq, Ord)

newtype Thrust = Thrust Float
    deriving stock (Show, Eq, Ord)

newtype Torque = Torque Float
    deriving stock (Show, Eq, Ord)

newtype Force = Force Float
    deriving stock (Show, Eq, Ord)

newtype Impulse = Impulse Float
    deriving stock (Show, Eq, Ord)

newtype AngularImpulse = AngularImpulse Float
    deriving stock (Show, Eq, Ord)

data PhysicalUnit
    = UnitTemperature Temperature
    | UnitPressure Pressure
    | UnitMass Mass
    | UnitVelocity Velocity
    | UnitThrust Thrust
    | UnitTorque Torque
    | UnitAngle Angle
    deriving stock (Show, Eq, Ord)