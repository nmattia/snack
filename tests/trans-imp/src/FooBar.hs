module FooBar where

import Conduit

data Baz = Baz Int

spitOut :: ConduitT () Int IO ()
spitOut = yieldMany [ 1 ..]

digest :: ConduitT Int Void IO ()
digest = mapM_C print
