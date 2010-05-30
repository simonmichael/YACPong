-----------------------------------------------------------------------------
--
-- Module      :  Draw
-- Copyright   :  Copyright (c) 2010 Korcan Hussein
-- License     :  MIT
--
-- Maintainer  :  korcan_h@hotmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
--
--    The MIT License
--
--    Copyright (c) 2010 Korcan Hussein.
--
--    Permission is hereby granted, free of charge, to any person obtaining a copy
--    of this software and associated documentation files (the "Software"), to deal
--    in the Software without restriction, including without limitation the rights
--    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--    copies of the Software, and to permit persons to whom the Software is
--    furnished to do so, subject to the following conditions:
--
--    The above copyright notice and this permission notice shall be included in
--    all copies or substantial portions of the Software.
--
--    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--    THE SOFTWARE.
--
{-# LANGUAGE FlexibleContexts #-}
module Draw (
    render,
    renderWin,
    renderPaused
) where

import Prelude hiding (id, (.), mod, init)
import Control.Category

import Control.Monad.Reader
import Control.Monad.Trans

import Data.Record.Label

import Graphics.UI.SDL hiding (flip)
import qualified Graphics.UI.SDL as SDL (flip)

import Graphics.UI.SDL.TTF

import Labels
import Surface
import GameState
import GameEnv
import Consts

render :: GameEnv ()
render = do
    screen <- askM screen
    liftIO $ clear screen 0x00 0x00 0x00

    drawNet
    drawPaddle 0
    drawPaddle 1
    drawBall

    drawScore

    liftIO $ SDL.flip screen

renderWin :: GameEnv ()
renderWin = do
    screen <- askM screen
    liftIO $ clear screen 0x00 0x00 0x00

    drawNet
    drawPaddle 0
    drawPaddle 1
    drawScore

    liftIO $ SDL.flip screen

renderPaused :: GameEnv ()
renderPaused = do
    screen <- askM screen
    liftIO $ clear screen 0x00 0x00 0x00

    --drawNet
    drawPaddle 0
    drawPaddle 1
    drawBall
    drawScore
    drawPaused
    liftIO $ SDL.flip screen

blitScreen :: (MonadIO m, MonadReader GameConfig m) => Int -> Int -> Surface -> Maybe Rect -> m ()
blitScreen x y s r = do
    screen <- askM screen
    applySurface' x y s screen r
{-# INLINE blitScreen #-}

drawNet :: GameEnv ()
drawNet = do
    dst <- askM screen
    liftIO $ drawLine dst (x,0) (screenHeight'-1) 2 Vertical $ Pixel 0xFFFFFFFF
 where screenHeight' = truncate screenHeight
       screenWidth'  = truncate screenWidth
       x = screenWidth' `div` 2


drawPaddle :: Int -> GameEnv ()
drawPaddle paddleIdx = do
    ps      <- askM paddleSprite
    (px,py) <- getM (pPos . paddlel)
    blitScreen (truncate px) (truncate py) ps Nothing
 where paddlel = if paddleIdx == 0 then paddle1 else paddle2

drawBall :: GameEnv ()
drawBall = do
    bs      <- askM ballSprite
    (bx,by) <- getM (pos . ball)
    blitScreen (truncate bx) (truncate by) bs Nothing

drawScore :: GameEnv ()
drawScore = do
    f   <- askM font
    p1c <- liftM show $ getM $ player1Count . stats
    p2c <- liftM show $ getM $ player2Count . stats

    p1Score <- liftIO $ renderTextSolid f p1c textColor
    p2Score <- liftIO $ renderTextSolid f p2c textColor

    blitScreen (halfWidth' - 50 - surfaceGetWidth p1Score) 20 p1Score Nothing
    blitScreen (halfWidth' + 50) 20 p2Score Nothing
 where
       halfWidth' = truncate halfWidth

drawPaused :: GameEnv ()
drawPaused = do
    pSprite <- askM pausedSprite
    blitScreen (halfWidth' - surfaceGetWidth pSprite `div` 2) (halfHeight' - surfaceGetHeight pSprite `div` 2) pSprite Nothing
 where
       halfWidth'  = truncate halfWidth
       halfHeight' = truncate halfHeight
