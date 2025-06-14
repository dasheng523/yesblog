module Fdd.LogicControl.Domain where

type Message = String
type Key = String

newtype LogicFailure = LogicFailure String
    deriving stock (Show, Eq, Ord)