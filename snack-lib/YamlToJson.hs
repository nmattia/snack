module Main where

import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as T
import System.Environment (getArgs)

main :: IO ()
main = do
    [file] <- getArgs
    yaml <- BS8.readFile file
    let Right value = Yaml.decodeEither' yaml :: Either Yaml.ParseException Aeson.Value
    BL8.putStrLn $ Aeson.encode value
