module TheOrderBook where

import PriorityQueue
import Lab2



data BuyBid     = BuyBid { bid :: Bid

}
data SellBid    = SellBid { bid2 :: Bid

}

type BuyOrderQueue  = SkewHeap BuyBid
type SellOrderQueue = SkewHeap SellBid

data OrderBook = OrderBook { buy :: BuyOrderQueue,
                            sell ::  SellOrderQueue }

--instance Ord BuyBid where
--    Compare BuyBid BuyBid = EQ
--    Compare BuyBid BuyBid = LT
--    Compare BuyBid BuyBid = LT


helperFunction :: OrderBook -> [Bid] -> IO()
helperFunction = undefined