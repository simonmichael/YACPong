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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GameEnv
(
    GameEnv(),
    runGame,
) where

import Control.Monad.State
import Control.Monad.Reader

import GameState

type GameState = StateT GameData IO
newtype GameEnv a = GameEnv { runEnv :: ReaderT GameConfig GameState a }
    deriving (Monad, MonadIO, MonadReader GameConfig, MonadState GameData)

runGame :: GameConfig -> GameData -> GameEnv a -> IO (a, GameData)
runGame config state (GameEnv env) = (runStateT . runReaderT env) config state
