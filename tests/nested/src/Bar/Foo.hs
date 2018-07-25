import Conduit
import Bar.FooBar

main :: IO ()
main = runConduit $ spitOut .| takeC 5 .| digest
