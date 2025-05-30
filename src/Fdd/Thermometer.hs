module Fdd.Thermometer where

data Therm

lookup :: String -> IO (Maybe Therm)
lookup = undefined

read :: Therm -> IO Float
read = undefined