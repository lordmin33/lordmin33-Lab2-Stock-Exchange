module TheOrderBook where

import PriorityQueue
import Lab2

data OrderBook a = Empty 
                | Node (SkewHeap a)


buyBid :: SkewHeap Bid
buyBid = undefined

sellBid :: SkewHeap Bid
sellBid = undefined


helperFunction :: OrderBook -> [Bid] -> IO
helperFunction = undefined