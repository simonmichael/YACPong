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

-----------------------------------------------------------------------------
--
-- Module      :  Sound
-- Copyright   :  Copyright (c) 2010 Korcan Hussein.
-- License     :  MIT
--
-- Maintainer  :  korcan_h@hotmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Sound (
    playWallSound,
    playPaddleSound,
    playWinSound,
    isPlayingWin
) where

import Control.Monad
import Control.Monad.Trans

import Data.Record.Label (askM)
import Labels
import GameState
import GameEnv

import Graphics.UI.SDL.Mixer

-- default number of channels available in SDL-mixer is 8
wallSoundID   = 0
paddleSoundID = 1
winSoundID    = 2

playWallSound :: GameEnv ()
playWallSound = do
    wb <- askM wallBounce
--    playing <- liftIO $ isChannelPlaying winSoundID
--    when (not playing) $ do
    liftIO $ playChannel wallSoundID wb 0
    return ()

playPaddleSound :: GameEnv ()
playPaddleSound = do
    pb <- askM paddleBounce
--    playing <- liftIO $ isChannelPlaying paddleSoundID
--    when (not playing) $ do
    liftIO $ playChannel paddleSoundID pb 0
    return ()

playWinSound :: GameEnv()
playWinSound = do
    ws <- askM winSound
    liftIO $ haltChannel winSoundID
    liftIO $ playChannel winSoundID ws 0
    return ()

isPlayingWin :: GameEnv Bool
isPlayingWin = do
    ws <- askM winSound
    currPlaying <- liftIO $ getChunk winSoundID
    isplaying <- liftIO $ isChannelPlaying winSoundID
    return $ ws == currPlaying && isplaying
