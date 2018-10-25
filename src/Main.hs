-- Based on http://andrew.gibiansky.com/blog/haskell/haskell-gloss/
-- Specify what to export for our doucmentation
module Main(main, PongGame(..), render, initialState) where

import Graphics.Gloss

width, height, offset :: Int
width = 300
height = 300
offset = 500

window :: Display
window = InWindow "1337 Window" (width, height) (offset, offset)

background :: Color
background = black

drawing :: Picture
drawing = circleSolid 20

main :: IO ()
main = display window background drawing

-- | Data describing the state of the pong game
data PongGame = Game 
    { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
    , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity.
    , player1 :: Float           -- ^ Left player paddle height.
                                 -- Zero is the middle of the screen.
    , player2 :: Float           -- ^ Right player paddle height.
    } deriving Show

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
    { ballLoc = (-10, 30)
    , ballVel = (1, -3)
    , player1 = 40
    , player2 = -80
    }

-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
    pictures 
    [ ball
    , walls
    , mkPaddle rose 120 $ player1 game
    , mkPaddle orange (-120) $ player2 game
    ]
    where
        --  The pong ball.
        ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
        ballColor = dark red

        --  The bottom and top walls.
        wall :: Float -> Picture
        wall offset =
            translate 0 offset $
            color wallColor $
            rectangleSolid 270 10

        wallColor = greyN 0.5
        walls = pictures [wall 150, wall (-150)]

        --  Make a paddle of a given border and vertical offset.
        mkPaddle :: Color -> Float -> Float -> Picture
        mkPaddle col x y = pictures
            [ translate x y $ color col $ rectangleSolid 26 86
            , translate x y $ color paddleColor $ rectangleSolid 20 80
            ]

        paddleColor = light (light blue)