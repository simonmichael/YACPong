{-
    The MIT License
    
    Copyright (c) 2010 Korcan Hussein
    
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
module Surface where

import Foreign

import Control.Monad
import Control.Monad.Trans

import Graphics.UI.SDL
import Graphics.UI.SDL.Image

isInside :: Rect -> Int -> Int -> Bool
isInside (Rect sx sy w h) x y =  x > sx && x < (sx + w) && y > sy && y < (sy + h)

loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey' colorKey

setColorKey' :: Maybe (Word8,Word8,Word8) -> Surface -> IO Surface
setColorKey' Nothing s = return s
setColorKey' (Just (r, g, b)) surface = mapRGB' surface r g b >>= setColorKey surface [SrcColorKey] >> return surface

applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO ()
applySurface x y src dst clip = blitSurface src clip dst offset >> return ()
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

applySurface' :: MonadIO m => Int -> Int -> Surface -> Surface -> Maybe Rect -> m ()
applySurface' x y src dst = liftIO . applySurface x y src dst

mapRGB' :: Surface -> Word8 -> Word8 -> Word8 -> IO Pixel
mapRGB' = mapRGB . surfaceGetPixelFormat

clear :: Surface -> Word8 -> Word8 -> Word8 -> IO ()
clear surf r g b = do
    jrect      <- Just `liftM` getClipRect surf
    clearColor <- mapRGB' surf r g b
    fillRect surf jrect clearColor
    return ()

getPixel32 :: Surface -> Int -> Int -> IO Pixel
getPixel32 s x y = do
    ps <- surfaceGetPixels s
    Pixel `liftM` peekElemOff (castPtr ps :: Ptr Word32) offset
 where offset = y * (fromIntegral $ surfaceGetPitch s `div` 4) + x

setPixel32 :: Surface -> Int -> Int -> Pixel -> IO ()
setPixel32 s x y (Pixel pixel) = do
    ps <- surfaceGetPixels s
    pokeElemOff (castPtr ps :: Ptr Word32) offset pixel
 where offset = y * (fromIntegral $ surfaceGetPitch s `div` 4) + x

withLock :: MonadIO m => Surface -> m () -> m ()
withLock s act = do
    liftIO $ lockSurface s
    act
    liftIO $ unlockSurface s

type Point2 = (Int,Int)
data LineDir = Vertical | Horizontal

drawLine :: Surface -> Point2 -> Int -> Int -> LineDir -> Pixel -> IO ()
drawLine dst (x,y) len thickness Horizontal colour =
    withLock dst $
        forM_ [(r,c) | c <- [yStart..yEnd], r <- [x..(x + len)]] $ \(r,c) ->
            setPixel32 dst r c colour
 where halfLen = thickness `div` 2
       yStart  = max (y - halfLen) 0
       yEnd    = min (yStart + thickness) $  surfaceGetHeight dst

drawLine dst (x,y) len thickness Vertical colour =
    withLock dst $
        forM_ [(r,c) | r <- [xStart..xEnd], c <- [y..(y + len)]] $ \(r,c) ->
            setPixel32 dst r c colour
 where halfLen = thickness `div` 2
       xStart  = max (x - halfLen) 0
       xEnd    = min (xStart + thickness) $ surfaceGetWidth dst
