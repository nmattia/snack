module Lib where

import Control.Lens
import Network.Wreq
import Data.Aeson.Lens
import Data.Text (Text)

topReddit :: IO Text
topReddit =
    getWith opts url
      <&> (^. responseBody
      . key "data"
      . key "children"
      . nth 0
      . key "data"
      . key "title"
      . _String)
  where
    url = "https://www.reddit.com/r/haskell/top.json"
    opts = defaults
      & param "limit" .~ ["1"]
      & param "t" .~ ["all"]
