module Fdd.Common.Value where

data Value
    = BoolValue Bool
    | IntValue Int
    | FloatValue Float
    | StringValue String

boolValue :: Bool -> Value
boolValue = BoolValue

stringValue :: String -> Value
stringValue = StringValue

intValue :: Int -> Value
intValue = IntValue

floatValue :: Float -> Value
floatValue = FloatValue