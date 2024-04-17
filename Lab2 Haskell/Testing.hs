-- where we make tests for Lab2

module Testing where

import Lab2
import PriorityQueue
import TheOrderBook


invariant :: Ord a => SkewHeap a -> Bool

