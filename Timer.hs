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
module Timer (Timer(), timer, start, stop, getTicks, isPaused) where

import Control.Monad.Trans

import qualified Graphics.UI.SDL.Time as SdlTime
import Data.Word

data Timer = Timer { startTicks :: Word32, stopTicks :: Word32 }

timer :: Timer
timer = Timer 0 0

start :: MonadIO m => Timer -> m Timer
start Timer { stopTicks=pausedTicks } = getTicksIO >>= \currTick -> return $ Timer (currTick - pausedTicks) 0

stop :: MonadIO m => Timer -> m Timer
stop t@Timer { startTicks = st }
    | isPaused t = return t
    | otherwise  = getTicksIO >>= \currTick -> return t { stopTicks=currTick - st }

getTicks :: MonadIO m => Timer -> m Word32
getTicks t@Timer { startTicks = st }
    | isPaused t = return $ stopTicks t
    | otherwise  = getTicksIO >>= \currTick -> return $ currTick - st

isPaused :: Timer -> Bool
isPaused Timer { stopTicks=st } = st > 0

getTicksIO :: MonadIO m => m Word32
getTicksIO = liftIO SdlTime.getTicks