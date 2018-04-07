import Data.Foo
import Haskell
import  qualified  FooBar
import  qualified  BarFoo as F

main = do
  putStrLn dataFoo
  putStrLn haskell
  putStrLn FooBar.fooBar
  putStrLn F.barFoo
