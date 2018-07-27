module Bar ( main, Baz(..)) where

import Conduit
import FooBar

main :: Baz -> IO ()
main (Baz n) = runConduit $ spitOut .| takeC n .| digest
