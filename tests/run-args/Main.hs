
-- Simple program that turns CLI args into lines

module Main (main) where

import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= mapM_ putStrLn
