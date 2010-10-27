{-# LANGUAGE ForeignFunctionInterface #-}
module MainWrapper where
import YACPong (main)
foreign export ccall "haskell_main" main :: IO ()
