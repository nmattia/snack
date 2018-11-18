module Main where

main :: IO ()
main = putStrLn "hello"

foo :: forall a. a -> a
foo = id
