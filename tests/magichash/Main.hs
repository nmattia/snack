{-# LANGUAGE MagicHash #-}

module Main (main, foo#) where

main :: IO ()
main = putStrLn $ "Hello" ++ show foo#

foo# :: Int
foo# = 2
