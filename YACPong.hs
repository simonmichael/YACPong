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
{-# LANGUAGE FlexibleContexts, ForeignFunctionInterface, CPP #-}
module YACPong where

import Prelude hiding (id, (.), mod, init)

import Foreign
import Foreign.C.Types

import Control.Category

import Control.Monad
import Control.Monad.State hiding (get)
import qualified Control.Monad.State as MState (get)
import Control.Monad.Reader
import Control.Monad.Trans

import Control.Exception

import Data.Maybe
import Data.Either
import Data.Record.Label
import Data.List hiding (init, intersect)

import Graphics.UI.SDL hiding (init, quit, flip, update, getTicks)
import qualified Graphics.UI.SDL as SDL (flip)
import qualified Graphics.UI.SDL.Utilities as Util

import Graphics.UI.SDL.TTF hiding (init, quit)
import qualified Graphics.UI.SDL.TTF as TTFG (init, quit)

import Graphics.UI.SDL.Mixer

import Labels
import Timer
import Vector2
import Collision
import Surface
import GameState
import GameEnv
import Consts
import Sound
import Draw
#ifdef MAKE
import Paths_YACPong_make
#else
import Paths_YACPong
#endif


-- SDL_GetKeyState is not defined in Graphic.UI.SDL
foreign import ccall unsafe "SDL_GetKeyState" sdlGetKeyState :: Ptr CInt -> IO (Ptr Word8)

type KeyProc = SDLKey -> Bool
-- this function comes from mokehehe's super nario bros: http://github.com/mokehehe/monao in the file "AppUtil.hs"
getKeyState :: MonadIO m => m KeyProc
getKeyState = liftIO $ alloca $ \numkeysPtr -> do
    keysPtr <- sdlGetKeyState numkeysPtr
    return $ \k -> (/= 0) $ unsafePerformIO $ (peekByteOff keysPtr $ fromIntegral $ Util.fromEnum k :: IO Word8)

init :: IO (GameConfig, GameData)
init = do
    screen  <- setVideoMode (truncate screenWidth) (truncate screenHeight) screenBpp [HWSurface, DoubleBuf]
    setCaption "YACPong" []
    enableUnicode True

    fontFileName   <- getDataFileName "Crysta.ttf"
    wallFileName   <- getDataFileName "wall.wav"
    paddleFileName <- getDataFileName "paddle.wav"
    winFileName    <- getDataFileName "win.wav"

    font         <- openFont fontFileName 36
    wallBounce   <- loadWAV wallFileName
    paddleBounce <- loadWAV paddleFileName
    winSound     <- loadWAV winFileName

    paddleSprite <- createRGBSurfaceEndian [HWSurface] paddleW' paddleH' screenBpp
    ballSprite   <- createRGBSurfaceEndian [HWSurface] ballW' ballH' screenBpp

    paddleColor <- mapRGB' paddleSprite 0xFF 0xFF 0x00
    ballColor   <- mapRGB' ballSprite 0x3F 0xFF 0xF5

    fillRect paddleSprite (Just $ Rect 0 0 paddleW' paddleH') paddleColor
    fillRect ballSprite (Just $ Rect 0 0 ballW' ballH') ballColor

    paused <- renderTextSolid font "PAUSED" textColor

    currTicks <- start timer

    return (GameConfig font screen ballSprite paddleSprite paused wallBounce paddleBounce winSound, gameData pPos1 pPos2 bPos currTicks)
 where paddleW' = truncate paddleW
       paddleH' = truncate paddleH
       ballW'   = truncate ballW
       ballH'   = truncate ballH
       --from (x,y) = (fromIntegral x, fromIntegral y)
       pPos1   = (10, halfHeight - paddleH / 2)
       pPos2   = (screenWidth - 10 - paddleW, halfHeight - paddleH / 2)
       bPos    = (halfWidth, halfHeight)

--whichSide :: Bounds -> Bounds -> Vector2f
--Bounds (lx,ly,lw,lh) `whichSide` Bounds (rx,ry,rw,rh) = flip execState (0,0) $ do
--    when ((lx + lw) <= rx) $ do
--        modify $ \(_,ny) -> (-1,ny)
--
--    when (lx >= (rx + rw)) $ do
--        modify $ \(_,ny) -> (1,ny)
--
--    when ((ly + lh) <= ry) $ do
--        modify $ \(nx,_) -> (nx,-1)
--
--    when (ly >= (ry + rh)) $ do
--        modify $ \(nx,_) -> (nx,1)

collide :: Float -> Paddle -> Ball -> Maybe (Float,Float)
collide dt Paddle { _pPos=(px,py), _yVel=yVel } b@Ball { _pos=(x,y), _vel=v } =
    intersectMoving (Bounds (px,py,pW,pH)) (Bounds (x,y,bW,bH)) (0, dt * yVel) (dt `mul` v)
  where bW = ballW
        bH = ballH
        pW = paddleW
        pH = paddleH

data CollisionType =
      CtWall (Either Paddle Ball)
    | CtPaddle Paddle
    | CtLeftNet
    | CtRightNet
    | CtNone

type CollisionEvent = (CollisionType, Float)

paddleInside :: Float -> Paddle -> Bool
paddleInside dt paddle =  not $ y' < 0 || y' + paddleH > screenHeight
 where (_,y) = getL pPos paddle
       vel   = getL yVel paddle
       y'    = vel * dt + y

findBallCollision :: Float -> [Paddle] -> Ball -> CollisionEvent
findBallCollision dt paddles b@Ball { _pos=bp@(x,y), _vel=v@(dx,dy) }
    | x' > 0 && x' < screenWidth && y' > 0 && y' < screenHeight = --b { _pos=(x',y') }
        let intersecting (_, Nothing) = False
            intersecting (_, Just (0,_)) = False
            intersecting _ =  True
            collided = find intersecting $ map (\p -> (p, collide dt p b)) paddles
        in case collided of
            Just (p, Just (t1,_)) -> (CtPaddle p, t1)
            _                     -> (CtNone, 0)
    | x' < 0            = (CtLeftNet, dt)
    | x' > screenWidth  = (CtRightNet, dt)
    | y' < 0            = (CtWall $ Right b, dt)
    | y' > screenHeight = (CtWall $ Right b, dt)
    | otherwise = (CtNone, 0)
 where x' = dx * dt + x
       y' = dy * dt + y

findCollisions :: Float -> GameEnv [CollisionEvent]
findCollisions dt = do
    p1 <- getM paddle1
    p2 <- getM paddle2
    let paddles = filter (not . paddleInside dt) [p1,p2]
    let wallPaddles = map (\p -> (CtWall $ Left p,dt)) paddles

    result <- findBallCollision dt [p1,p2] `liftM` getM ball
    case result of
        (CtNone,_) -> return wallPaddles
        _          -> return $ result : wallPaddles

think :: Ball -> Vector2f -> Paddle  -> Paddle
think (Ball (bx,by) bv) pv p@Paddle { _pPos=(x,y), _yVel=pVel, _lastPos=oldY }
    | ang < 0   =
        let newVel = if by < y then -paddleVel
                     else if by > y + paddleH
                        then paddleVel
                        else 0
            p' = saveLastPos newVel p -- capture the first y-pos of the paddle.
        in p' { _yVel=newVel  }
    | otherwise = setL yVel 0 p
 where ang = bv `dot` pv

react :: CollisionEvent -> GameEnv ()
react (CtWall (Left p), _) = do
    p1 <- getM paddle1

    let playerL = if p1 == p then paddle1 else paddle2
    modM playerL $ \p@Paddle { _pPos=(x,y) } ->
        p { _pPos=(min w $ max x 0, min h $ max y 0) }
 where w = screenWidth
       h = screenHeight

react (CtWall (Right b@Ball { _pos=(x,y), _vel=(dx,dy) }), dt) = do
    setM ball b { _pos=pos', _vel=reflect (dx,dy) wallNormal }
    playWallSound
 where x' = x + dt * dx
       y' = y + dt * dy
       (pos', wallNormal) = execState collideWall ((x',y'),(0,0))
       collideWall :: State (Vector2f,Vector2f) ()
       collideWall = do
        --modify $ \k@((u,v),(_,ny)) -> if u < 0 then ((0,v),(1,ny)) else k
        --modify $ \k@((u,v),(_,ny)) -> if u > fromIntegral screenWidth then ((fromIntegral $ screenWidth - 1, v),(-1,ny)) else k
        modify $ \k@((u,v),(nx,_)) -> if v < 0 then ((u, 0),(nx,1)) else k
        modify $ \k@((u,v),(nx,_)) -> if v > screenHeight then ((u, screenHeight - 1),(nx,-1)) else k

react (CtPaddle p, t) = do
    modM ball $ \b@Ball { _pos=bp, _vel=v } ->
        let v' = t `mul` normalize v
            p' = bp `add` v'
            --n        = whichSide (i,j,bW,bH) (px,py + dt * yVel,pW,pH)
        --in b { _pos=p', _vel=(0, paddleVel) `add` reflect v (1,0) }
        in b { _pos=p', _vel=paddleInflunceVec `add` reflect v (1,0) }
    playPaddleSound
 where currY     = snd $ getL pPos p
       paddleVel = getL yVel p
       lastY     = getL lastPos p
       paddleInflunceVec = if lastY == 0 then (0,0) else (0, currY - lastY)

react (CtLeftNet,_) = do
    state =: Win Player1
    modM (player2Count . stats) (+1)
    playWinSound

react (CtRightNet, _) = do
    state =: Win Player2
    modM (player1Count . stats) (+1)
    playWinSound

react (CtNone, _) = return ()

updatePaddle :: Float -> Paddle -> Paddle
updatePaddle dt paddle = setL pPos (x,y') paddle
 where (x,y) = getL pPos paddle
       vel   = getL yVel paddle
       y'    = vel * dt + y

updateBall :: Float -> Ball -> Ball
updateBall dt b@Ball { _pos=bp@(x,y), _vel=v@(dx,dy) } = b { _pos=(x',y') }
 where x' = dx * dt + x
       y' = dy * dt + y

saveLastPos :: Float -> Paddle -> Paddle
saveLastPos nextVel p
    | currVel == 0 && nextVel /= 0 = p { _lastPos = snd $ getL pPos p }
    | currVel /= 0 && nextVel == 0 = p { _lastPos = 0 }
    | otherwise                    = p
 where currVel = getL yVel p

update :: Float -> GameEnv ()
update dt = do

    keyState <- getKeyState

    let pVel = if keyState SDLK_w
                then -paddleVel
               else if (keyState SDLK_s)
                then paddleVel
                else 0.0

    -- capture the first y-pos of the player's paddle.
    modM paddle1 $ saveLastPos pVel

    setM (yVel . paddle1) pVel

    b <- getM ball
    modM paddle2 $ think b (-1,0)

    collisions <- findCollisions dt

    forM_ collisions react

    when (all ballNotHit collisions) $
        modM ball $ updateBall dt

    p1 <- getM paddle1
    when (all (paddleNotHit p1) collisions) $
        modM paddle1 $ updatePaddle dt

    p2 <- getM paddle2
    when (all (paddleNotHit p2) collisions) $
        modM paddle2 $ updatePaddle dt

 where ballNotHit :: CollisionEvent -> Bool
       ballNotHit (CtWall (Left _), _) = True
       ballNotHit (CtNone, _)          = True
       ballNotHit _                    = False

       paddleNotHit :: Paddle -> CollisionEvent -> Bool
       paddleNotHit p (CtWall (Left c), _)  = p /= c
       paddleNotHit _ (CtWall (Right _), _) = True
       paddleNotHit _ (CtNone, _)           = True
       paddleNotHit _  _                    = False

nextState_ :: GameLoopState -> GameEnv ()
nextState_ w@(Win player) = do
    renderWin
    isWinPlay <- isPlayingWin
    stopTicks
    let result = if isWinPlay then w else Init player
    state =: result

nextState_ (Init player) = do
    setM ball =<< newBallM player (halfWidth, halfHeight)
    state =: Play

nextState_ Play = do
    dt <- getTicksM
    update $ secs * fromIntegral dt
    resetTicks
    render

nextState_ Paused = do
    renderPaused
    stopTicks

nextState :: GameEnv ()
nextState = getM state >>= nextState_

loop :: GameEnv ()
loop = do
    quit <- whileEvents $ \event -> do
        gs <- getM state
        case (event, gs) of
            (KeyDown (Keysym SDLK_p _ _), Play)   -> setM state Paused
            (KeyDown (Keysym SDLK_p _ _), Paused) -> setM state Play
            _ -> return ()

    nextState

    -- TODO: use high res performance timer.
    liftIO $ delay 1

    unless quit loop

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit    -> return True
        KeyDown (Keysym SDLK_ESCAPE _ _) -> return True
        NoEvent -> return False
        _       ->  do
            act event
            whileEvents act

main :: IO ()
main = withSDL $ do
        (config, state) <- init
        runGame config state loop
        return ()

withSDL :: IO () -> IO ()
withSDL act = withInit [InitEverything] $ do -- withInit calls quit for us.
            bracket_ sdlInit sdlQuit act
 where sdlInit :: IO ()
       sdlInit = do
            success <- TTFG.init
            when (not success) $
                throwIO $ userError "Failed to init ttf\n"
            openAudio 44100 AudioS16Sys 2 4096
       sdlQuit :: IO ()
       sdlQuit = closeAudio >> TTFG.quit
