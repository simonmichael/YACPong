{-
    The MIT License
    
    Copyright (c) 2010 Korcan Hussein.
    
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:
    
    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.
    
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    THE SOFTWARE.
-}
{-# LANGUAGE TemplateHaskell, TypeOperators, FlexibleContexts #-}
module GameState where

import Prelude hiding (id, (.), mod)
import Control.Category
import Control.Monad.Reader
import Control.Monad.State hiding (get)

import Data.Word
import Data.Record.Label

import Graphics.UI.SDL (Surface)
import Graphics.UI.SDL.TTF (Font)
import Graphics.UI.SDL.Mixer (Chunk)

import Labels
import Vector2
import Collision
import Timer

--type Vector2f = (Float, Float)
data Player = Player1 | Player2
    deriving (Eq, Show)

data GameLoopState =
      Win Player
    | Init
    | Play
    | Paused

data Ball = Ball {
    _pos :: Vector2f,
    _vel :: Vector2f
} deriving (Eq, Show)

data Paddle = Paddle {
    _pPos :: Vector2f,
    _yVel :: Float
} deriving (Eq, Show)

data Stats = Stats {
    _player1Count :: Integer,
    _player2Count :: Integer 
} deriving (Eq, Show)

data GameData = GameData {
    _paddle1   :: Paddle,
    _paddle2   :: Paddle,
    _ball      :: Ball,
    _stats     :: Stats,
    --_currTick  :: Word32,
    _ticker    :: Timer,
    _state     :: GameLoopState
}

data GameConfig = GameConfig {
    _font         :: Font,
    _screen       :: Surface,
    _ballSprite   :: Surface,
    _paddleSprite :: Surface,
    _pausedSprite :: Surface,
    _wallBounce   :: Chunk,
    _paddleBounce :: Chunk,
    _winSound     :: Chunk
}

$(mkLabels [''Ball, ''Paddle, ''Stats, ''GameData, ''GameConfig])

getTicksM :: (MonadIO m, MonadState GameData m) => m Word32
getTicksM = getM ticker >>= getTicks

stopTicks :: (MonadIO m, MonadState GameData m) => m ()
stopTicks = modM_ ticker stop

startTicks :: (MonadIO m, MonadState GameData m) => m ()
startTicks = modM_ ticker start

resetTicks :: (MonadIO m, MonadState GameData m) => m ()
resetTicks = start timer >>= setM ticker

-- (-141.4,-141.4)
-- (200.0,0)
newBall :: Vector2f -> Ball
newBall pos = Ball pos (-141.4,-141.4)

gameData :: Vector2f -> Vector2f -> Vector2f -> Timer -> GameData
gameData p1 p2 b ticks = GameData (Paddle p1 0) (Paddle p2 0) (Ball b (200.0,0)) (Stats 0 0) ticks Init

mapPlayer :: Player -> Stats :-> Integer
mapPlayer Player1 = player1Count
mapPlayer Player2 = player2Count
