{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.FileEmbed
import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main = BS8.putStrLn $(embedFile "foo.txt")
