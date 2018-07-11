module Main where

import Lib

main :: IO ()
main = topReddit >>= print
