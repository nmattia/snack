module Bar where

import Conduit
import FooBar

main :: IO ()
main = runConduit $ spitOut .| takeC 5 .| digest
