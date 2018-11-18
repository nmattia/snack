module Bar where

import Conduit

spitOut :: ConduitT () Int IO ()
spitOut = yieldMany [ 1 ..]

digest :: ConduitT Int Void IO ()
digest = mapM_C print
