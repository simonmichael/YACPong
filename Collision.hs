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
{-# LANGUAGE MultiParamTypeClasses #-}
module Collision where

import Vector2

class Collision a b where
    intersect :: a -> b -> Bool

newtype Bounds = Bounds { sides :: (Float,Float,Float,Float) }

instance Collision Bounds Bounds where
    Bounds (ax, ay, aw, ah) `intersect` Bounds (bx, by, bw, bh)
        | t1 > bw || (-t1) > aw = False
        | t2 > bh || (-t2) > ah = False
        | otherwise = True  
     where t1 = ax - bx
           t2 = ay - by

-- Dynamic seperating-axis test
intersectMoving :: Bounds -> Bounds -> Vector2f -> Vector2f -> Maybe (Float,Float)
intersectMoving a@(Bounds (ax, ay, aw, ah)) b@(Bounds (bx, by, bw, bh)) va vb
    | intersect a b = Just (0,0)
    | otherwise = do
        let v = vb `sub` va
        -- test x-axis
        (t1, t2) <- test (fst v) 0 1 ax maxAx bx maxBx        
        -- test y-axis
        test (snd v) t1 t2 ay maxAy by maxBy

 where maxAx = ax + aw
       maxAy = ay + ah
       maxBx = bx + bw
       maxBy = by + bh
       test vi tfirst tlast aMin aMax bMin bMax =
            case test1 vi tfirst tlast aMin aMax bMin bMax of
                Just (t1, t2) -> if t1 > t2 then Nothing else Just (t1,t2)
                _ -> Nothing
       test1 vi tfirst tlast aMin aMax bMin bMax
        | vi < 0.0 = if bMax < aMin
                    then Nothing -- no-interesection will occur
                    else
                        let t1 = if aMax < bMin then max ((aMax - bMin) / vi) tfirst else tfirst
                            t2 = if bMax > aMin then min ((aMin - bMax) / vi) tlast else tlast
                        in Just (t1,t2)
        | vi > 0.0 = if bMin > aMax
                    then Nothing -- no-interesection will occur
                    else
                        let t1 = if bMax < aMin then max ((aMin - bMax) / vi) tfirst else tfirst
                            t2 = if aMax > bMin then min ((aMax - bMin) / vi) tlast else tlast
                        in Just (t1,t2)
        | otherwise = -- zero component
                    if bMax < aMin || bMin > aMax
                        then Nothing -- no-interesection
                        else Just (max 0.0 tfirst, min 1.0 tlast) 