-- Based on http://andrew.gibiansky.com/blog/haskell/haskell-gloss/
-- Specify what to export for our doucmentation
module Main
  ( main
  , PongGame(..)
  , render
  , initialState
  ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

width, height, offset :: Int
width = 300

height = 300

offset = 500

-- | Window Properties
window :: Display
window = InWindow "Haskell Pong" (width, height) (offset, offset)

-- | Window color
background :: Color
background = black

-- | Number of frames to show per second.
fps :: Int
fps = 60

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState =
  Game {ballLoc = (-10, 30), ballVel = (80, -110), player1 = 40, player2 = -80}

-- | Initialize the main function
main :: IO ()
main = play window background fps initialState render handleKeys update

-- | Data describing the state of the pong game
data PongGame = Game
  { ballLoc :: (Float, Float) -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float) -- ^ Pong ball (x, y) velocity.
  , player1 :: Float -- ^ Left player paddle height.
                     -- Zero is the middle of the screen.
  , player2 :: Float -- ^ Right player paddle height.
  } deriving (Show)

-- | Convert a game state into a picture.
render ::
     PongGame -- ^ The game state to render.
  -> Picture -- ^ A picture of this game state.
render game =
  pictures
    [ ball
    , walls
    , mkPaddle white 120 $ player1 game
    , mkPaddle white (-120) $ player2 game
    ]
      --  The pong ball.
  where
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red
      --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset = translate 0 offset $ color wallColor $ rectangleSolid 300 10
    wallColor = greyN 0.9
    walls = pictures [wall 150, wall (-150)]
      --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y =
      pictures
        [ translate x y $ color col $ rectangleSolid 26 86
        , translate x y $ color paddleColor $ rectangleSolid 20 80
        ]
    paddleColor = greyN 0.9

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: Float -> PongGame -> PongGame
update seconds = paddleBounce . wallBounce . moveBall seconds

-- | Update the ball position using its current velocity.
moveBall ::
     Float -- ^ The number of seconds since last update
  -> PongGame -- ^ The initial game state
  -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game {ballLoc = (x', y')}
    -- Old locations and velocities.
  where
    (x, y) = ballLoc game
    (vx, vy) = ballVel game
    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game {ballVel = (vx, vy')}
    -- Radius. Use the same thing as in `render`.
  where
    radius = 10
    -- The old velocities.
    (vx, vy) = ballVel game
    vy' =
      if wallCollision (ballLoc game) radius
             -- Update the velocity.
        then -vy
            -- Do nothing. Return the old velocity.
        else vy

type Radius = Float

type Position = (Float, Float)

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision = y - radius <= -fromIntegral height / 2
    bottomCollision = y + radius >= fromIntegral height / 2

-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game {ballVel = (vx', vy)}
    -- Radius. Use the same thing as in `render`.
  where
    radius = 10
    -- The old velocities.
    (vx, vy) = ballVel game
    vx' =
      if paddleCollision (ballLoc game) (player1 game) (player2 game) radius
             -- Update the velocity.
        then -vx
            -- Do nothing. Return the old velocity.
        else vx

-- | Given position and radius of the ball, return whether a collision occurred.
paddleCollision :: Position -> Float -> Float -> Radius -> Bool
paddleCollision (x, y) player1 player2 radius = leftCollision || rightCollision
  where
    leftCollision =
      x - radius <= -fromIntegral width / 2 + 43 &&
      y >= player2 - 43 - radius && y <= player2 + 43 + radius
    rightCollision =
      x + radius >= fromIntegral width / 2 - 43 &&
      y >= player1 - 43 - radius && y <= player1 + 43 + radius

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame
-- For a 'r' keypress, reset the ball to the center.
handleKeys (EventKey (Char 'r') _ _ _) game = game {ballLoc = (0, 0)}
handleKeys (EventKey (SpecialKey KeyUp) _ _ _) game =
  game {player1 = (player1 game) + 30.0}
handleKeys (EventKey (SpecialKey KeyDown) _ _ _) game =
  game {player1 = (player1 game) - 30.0}
handleKeys (EventKey (Char 'w') _ _ _) game =
  game {player2 = (player2 game) + 30.0}
handleKeys (EventKey (Char 's') _ _ _) game =
  game {player2 = (player2 game) - 30.0}
-- Do nothing for all other events.
handleKeys _ game = game
