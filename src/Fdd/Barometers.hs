module Fdd.Barometers where

data Bar

lookup :: String -> IO (Maybe Bar)
lookup = undefined

read :: Bar -> IO Float
read = undefined