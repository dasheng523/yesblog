module Ys.Yampa.Cube.Input (
    AppInput,
    parseWinInput,
    mousePos,
    lbp,
    lbpPos,
    lbDown,
    rbp,
    rbpPos,
    rbDown,
    keyPress,
    keyPressed,
    quitEvent,
    module SDL.Input.Keyboard.Codes,
) where

import FRP.Yampa

import Linear (V2 (..))
import Linear.Affine (Point (..))

import qualified SDL
import SDL.Input.Keyboard.Codes

import Ys.Yampa.Cube.Types
import Prelude hiding (first, second)

-- <| Signal Functions |> --

-- | Current mouse position
mousePos :: SF AppInput (Double, Double)
mousePos = arr inpMousePos

-- | Events that indicate left button click
lbp :: SF AppInput (Event ())
lbp = lbpPos >>^ tagWith ()

-- | Events that indicate left button click and are tagged with mouse position
lbpPos :: SF AppInput (Event (Double, Double))
lbpPos = inpMouseLeft ^>> edgeJust

-- | Is left button down
lbDown :: SF AppInput Bool
lbDown = arr (isJust . inpMouseLeft)

-- | Events that indicate right button click
rbp :: SF AppInput (Event ())
rbp = rbpPos >>^ tagWith ()

-- | Events that indicate right button click and are tagged with mouse position
rbpPos :: SF AppInput (Event (Double, Double))
rbpPos = inpMouseRight ^>> edgeJust

-- | Is right button down
rbDown :: SF AppInput Bool
rbDown = arr (isJust . inpMouseRight)

keyPress :: SF AppInput (Event SDL.Scancode)
keyPress = inpKeyPressed ^>> edgeJust

keyPressed :: SDL.Scancode -> SF AppInput (Event ())
keyPressed code = keyPress >>^ filterE (code ==) >>^ tagWith ()

quitEvent :: SF AppInput (Event ())
quitEvent = arr inpQuit >>> edge

-- | Exported as abstract type. Fields are accessed with signal functions.
data AppInput = AppInput
    { inpMousePos :: (Double, Double)
    -- ^ Current mouse position
    , inpMouseLeft :: Maybe (Double, Double)
    -- ^ Left button currently down
    , inpMouseRight :: Maybe (Double, Double)
    -- ^ Right button currently down
    , inpKeyPressed :: Maybe SDL.Scancode
    , inpQuit :: Bool
    -- ^ SDL's QuitEvent
    }

initAppInput :: AppInput
initAppInput =
    AppInput
        { inpMousePos = (0, 0)
        , inpMouseLeft = Nothing
        , inpMouseRight = Nothing
        , inpKeyPressed = Nothing
        , inpQuit = False
        }

{- | Filter and transform SDL events into events which are relevant to our
  application
-}
parseWinInput :: SF WinInput AppInput
parseWinInput = accumHoldBy nextAppInput initAppInput

{- | Compute next input
  FIXME: I am reinventing lenses once again
-}
nextAppInput :: AppInput -> SDL.EventPayload -> AppInput
nextAppInput inp SDL.QuitEvent = inp{inpQuit = True}
nextAppInput inp (SDL.MouseMotionEvent ev) =
    inp{inpMousePos = (fromIntegral x, fromIntegral y)}
  where
    P (V2 x y) = SDL.mouseMotionEventPos ev
nextAppInput inp (SDL.KeyboardEvent ev)
    | SDL.keyboardEventKeyMotion ev == SDL.Pressed =
        inp{inpKeyPressed = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym ev}
    | SDL.keyboardEventKeyMotion ev == SDL.Released =
        inp{inpKeyPressed = Nothing}
nextAppInput inp (SDL.MouseButtonEvent ev) =
    inp
        { inpMouseLeft = lmb
        , inpMouseRight = rmb
        }
  where
    motion = SDL.mouseButtonEventMotion ev
    button = SDL.mouseButtonEventButton ev
    pos = inpMousePos inp
    inpMod = case (motion, button) of
        (SDL.Released, SDL.ButtonLeft) -> first (const Nothing)
        (SDL.Pressed, SDL.ButtonLeft) -> first (const (Just pos))
        (SDL.Released, SDL.ButtonRight) -> second (const Nothing)
        (SDL.Pressed, SDL.ButtonRight) -> second (const (Just pos))
        _ -> id
    (lmb, rmb) = inpMod $ (inpMouseLeft &&& inpMouseRight) inp
nextAppInput inp _ = inp