module Algorithms.Htn
( Term
, PrimitiveTask
, CompoundTask
, Task(..)
, Domain(..)
, htn
) where

import Algorithms.Htn.Internal

htn :: (PrimitiveTask a, CompoundTask b, Term c) => Domain a b c -> [c] -> [Task a b] -> ([Task a b], [c])
htn domain condition tasks = htn' domain condition tasks []
