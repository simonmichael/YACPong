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

screenBpp :: Int
screenBpp    = 32

screenWidth, screenHeight :: Float
screenWidth  = 640
screenHeight = 480


halfHeight, halfWidth :: Float
halfHeight = screenHeight / 2
halfWidth  = screenWidth / 2

paddleW, paddleH, ballW, ballH :: Float
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

degToRad :: Float
degToRad = pi / 180

ballVel :: Float
ballVel = 350
