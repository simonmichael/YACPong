-----------------------------------------------------------------------------
--
-- Module      :  Consts
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
module Consts where

import Graphics.UI.SDL.Color

screenWidth, screenHeight, screenBpp:: Int
screenWidth  = 640
screenHeight = 480
screenBpp    = 32

halfHeight, halfWidth :: Int
halfHeight = screenHeight `div` 2
halfWidth  = screenWidth `div` 2

paddleW, paddleH, ballW, ballH :: Int
paddleW = 22
paddleH = 96
ballW   = 8
ballH   = 8

paddleVel :: Float
paddleVel = 400

secs :: Float
secs = 1.0 / 1000.0

textColor :: Color
textColor = Color 0xFF 0xFF 0xFF
