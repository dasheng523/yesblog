module Ptld.Ch01 where

import qualified Data.Map as Map

data Cell
    = Alive
    | Dead
    deriving stock (Show, Eq)

type Coords = (Int, Int)
type Board = Map.Map Coords Cell

newtype GoL = GoL Board
newtype Seeds = Seeds Board
newtype Replicator = Replicator Board