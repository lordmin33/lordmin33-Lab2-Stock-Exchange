module TheOrderBook where

import PriorityQueue
import Lab2

-- defently  wrong
data OrderBook a = Empty 
                | Node (SkewHeap a)


type BuyBid a = BuyBid (SkewHeap a)
type SellBid a = SellBid (SkewHeap a)



helperFunction :: OrderBook -> [Bid] -> IO
helperFunction = undefined