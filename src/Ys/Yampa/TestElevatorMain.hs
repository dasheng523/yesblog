{- |
Description : Testing of the Elevator simulator.
Copyright   : The University of Nottingham, 2004
 Authors    : Henrik Nilsson

Part of Elevator example.
-}
module Ys.Yampa.TestElevatorMain where

import FRP.Yampa
import Text.Printf (printf)
import Ys.Yampa.Elevator
import Prelude hiding (second)

smplPer :: DTime
smplPer = 0.01

lbps :: SF a (Event ())
lbps = afterEach [(3.0, ()), (2.0, ()), (50.0, ())]

rbps :: SF a (Event ())
rbps = afterEach [(20.0, ()), (2.0, ()), (18.0, ()), (15.001, ())]

-- Looks for interesting events by inspecting the input events
-- and the elevator position over the interval [0, t_max].

data State = Stopped | GoingUp | GoingDown deriving stock (Eq)

testElevator :: Time -> [(Time, ((Event (), Event ()), Position))]
testElevator t_max = takeWhile ((<= t_max) . fst) tios
 where
  -- Time, Input, and Output
  tios =
    embed
      (localTime &&& ((lbps &&& rbps >>^ dup) >>> second elevator))
      (deltaEncode smplPer (repeat ()))

findEvents ::
  [(Time, ((Event (), Event ()), Position))] ->
  [(Time, Position, String)]
findEvents [] = []
findEvents tios@((_, (_, y)) : _) = feAux Stopped y tios
 where
  feAux _ _ [] = []
  feAux sPre yPre ((t, ((lbp, rbp), y')) : tios') =
    if not (null message)
      then (t, y', message) : feAux s y' tios'
      else feAux s y' tios'
   where
    s
      | y' == yPre = Stopped
      | yPre < y' = GoingUp
      | otherwise = GoingDown

    ms =
      if s /= sPre
        then case s of
          Stopped -> Just "elevator stopped"
          GoingUp -> Just "elevator started going up"
          GoingDown -> Just "elevator started going down"
        else Nothing

    mu =
      if isEvent lbp
        then Just "up button pressed"
        else Nothing

    md =
      if isEvent rbp
        then Just "down button pressed"
        else Nothing

    message = intercalate ", " (catMaybes [ms, mu, md])

formatEvent :: (Time, Position, String) -> String
formatEvent (t, y, m) = "t = " ++ t' ++ ",\ty = " ++ y' ++ ":\t" ++ m
 where
  t' = printf "%.2f" t
  y' = printf "%.2f" y

ppEvents :: [(Time, Position, String)] -> IO ()
ppEvents = foldr ((>>) . putStrLn . formatEvent) pass

testElevatorMain :: IO ()
testElevatorMain = ppEvents (findEvents (testElevator 100))
