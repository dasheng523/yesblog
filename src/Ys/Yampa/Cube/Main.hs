{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Ys.Yampa.Cube.Main where

import FRP.Yampa

import Ys.Yampa.Cube.Graphics
import Ys.Yampa.Cube.Input
import Ys.Yampa.Cube.Shapes
import Prelude hiding ((&))

-- < Constants > ---------------------------------------------------------------

cubeX, cubeWidth, cubeHeight :: Double
cubeX = 100
cubeWidth = 30
cubeHeight = 30

pipeWidth, pipeGap :: Double
pipeWidth = 60
pipeGap = 200

cubeColour, pipeColour, skyColour, groundColour :: Colour Double
cubeColour = sRGB24 0xED 0xBA 0x00
pipeColour = sRGB24 0x1A 0xAF 0x5D
skyColour = sRGB24 0xAD 0xD4 0xF4
groundColour = sRGB24 0xCE 0xB1 0x71

winHeight, winWidth :: Double
winHeight = 600
winWidth = 300

groundHeight :: Double
groundHeight = winHeight / 8

g :: Double
g = 200

-- < Game State > --------------------------------------------------------------

data Cube = Cube
    { cubePos :: Double
    , cubeVel :: Double
    }

initCube :: Cube
initCube = Cube (winHeight / 2) 0

data Pipe = Pipe
    { pipePos :: Double
    , pipeHeight :: Double
    }
initPipe :: Pipe
initPipe = Pipe winWidth 100

data Game = Game
    { gameCube :: Cube
    , gamePipe :: Pipe
    }

initGame :: Game
initGame = Game initCube initPipe

-- < Game logic > --------------------------------------------------------------

game :: SF AppInput Game
game = switch sf (const game)
  where
    sf = proc input -> do
        gameState <- gameSession -< input
        gameOver <- edge -< checkCollision gameState
        returnA -< (gameState, gameOver)

checkCollision :: Game -> Bool
checkCollision (Game (Cube cubeY _) (Pipe pipeX pipeHeight')) =
    collide (pipeHeight', pipeHeight')
        || collide (winHeight, winHeight - pipeHeight' - pipeGap)
        || (cubeY <= groundHeight + cubeHeight)
  where
    collide (y2, h2) =
        and
            [ cubeX + cubeWidth > pipeX
            , cubeX < pipeX + pipeWidth
            , cubeY > y2 - h2
            , cubeY - cubeHeight < y2
            ]

gameSession :: SF AppInput Game
gameSession = proc input -> do
    cube <- flappingCube initCube -< input
    pipe <- movingPipe initPipe -< ()
    returnA -< Game cube pipe

fallingCube :: Cube -> SF a Cube
fallingCube (Cube y0 v0) = proc _ -> do
    v <- imIntegral v0 -< -g
    y <- imIntegral y0 -< v
    returnA -< Cube y v

flappingCube :: Cube -> SF AppInput Cube
flappingCube cube0 = switch sf cont
  where
    sf = proc input -> do
        cube <- fallingCube cube0 -< ()
        flap' <- flapTrigger -< input
        returnA -< (cube, flap' `tag` cube)
    cont (Cube y v) = flappingCube (Cube y (v + 300))

movingPipe :: Pipe -> SF a Pipe
movingPipe (Pipe p0 h0) = switch sf (\_ -> movingPipe $ Pipe p0 h0)
  where
    sf = proc _ -> do
        p <- imIntegral p0 -< -100
        respawn <- edge -< p < -pipeWidth
        returnA -< (Pipe p h0, respawn)

-- < Rendering > ---------------------------------------------------------------

render :: Game -> Object
render (Game (Cube cubeY _) (Pipe pipeX pipeHeight')) = scene & colour_ skyColour
  where
    scene = scene_ [ground, cube, bottomPipe, upperPipe]
    ground =
        rectangle_ winWidth groundHeight
            & pos_ (0, groundHeight)
            & colour_ groundColour
    cube =
        rectangle_ cubeWidth cubeHeight
            & pos_ (cubeX, cubeY)
            & colour_ cubeColour
    bottomPipe =
        rectangle_ pipeWidth pipeHeight'
            & pos_ (pipeX, pipeHeight')
            & colour_ pipeColour
    upperPipe =
        rectangle_ pipeWidth (winHeight - pipeGap - pipeHeight')
            & pos_ (pipeX, winHeight)
            & colour_ pipeColour

-- < Input handling > ----------------------------------------------------------

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

flapTrigger :: SF AppInput (Event ())
flapTrigger = proc input -> do
    mouseTap <- lbp -< input
    spacebarTap <- keyPressed ScancodeSpace -< input
    returnA -< mouseTap `lMerge` spacebarTap

-- < Main Function > -----------------------------------------------------------

runCubeMain :: IO ()
runCubeMain =
    animate
        "Yampy cube"
        (round winWidth)
        (round winHeight)
        (parseWinInput >>> ((game >>^ render) &&& handleExit))
