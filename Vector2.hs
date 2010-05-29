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
module Vector2 where

type Vector2f = (Float,Float)

magSq,mag :: Vector2f -> Float
normalize :: Vector2f -> Vector2f
mul :: Float -> Vector2f -> Vector2f
add :: Vector2f -> Vector2f -> Vector2f
sub :: Vector2f -> Vector2f -> Vector2f
dot :: Vector2f -> Vector2f -> Float
reflect :: Vector2f -> Vector2f -> Vector2f

s `mul` (x, y) =  (s * x, s * y)

(lx, ly)`sub` (rx, ry) = (lx - rx, ly - ry)

(lx, ly)`add` (rx, ry) = (lx + rx, ly + ry)

(lx, ly) `dot` (rx, ry) = (lx * rx) + (ly * ry)

reflect v n = v `sub` (2 ` mul` ((n `dot` v) `mul` n))

magSq v = v `dot` v
mag v = sqrt $ magSq v

normalize v = len `mul` v
 where len = 1.0 / mag v
