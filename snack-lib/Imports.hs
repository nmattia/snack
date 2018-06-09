{-# LANGUAGE ViewPatterns #-}

module Main where

import Language.Haskell.Exts
import System.Environment

main :: IO ()
main = do
  [x] <- getArgs
  ParseOk mod <- parseFile x
  ParseOk (Module _ _ _ imps _) <- parseFile x
  let modNames = (\(importModule -> (ModuleName _ name)) -> name) <$> imps
  -- this is "JSON"
  print modNames
