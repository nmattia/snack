{-# LANGUAGE ViewPatterns #-}

module Main where

import Language.Haskell.Exts
import System.Environment
import Control.Exception

main :: IO ()
main = do
  [x] <- getArgs
  str <- readFile x
  imps <- case parse str :: ParseResult (NonGreedy (ModuleHeadAndImports SrcSpanInfo)) of
    ParseOk (unNonGreedy -> ModuleHeadAndImports _ _ _ imps) -> pure imps
    x -> throwIO $ userError $ show x
  let modNames = (\(importModule -> (ModuleName _ name)) -> name) <$> imps
  -- this is "JSON"
  print modNames
