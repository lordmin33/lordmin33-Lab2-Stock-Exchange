import Data.List
import Control.Monad
import PriorityQueue

-- Define types for BuyBid and SellBid
type BuyBid = SkewHeap Bid
type SellBid = SkewHeap Bid

-- Implement Ord instances for BuyBid and SellBid
instance Ord BuyBid where
  compare Empty Empty = EQ
  compare Empty _ = LT
  compare _ Empty = GT
  compare (Node (Buy _ price1) _ _) (Node (Buy _ price2) _ _) = compare price2 price1

instance Ord SellBid where
  compare Empty Empty = EQ
  compare Empty _ = LT
  compare _ Empty = GT
  compare (Node (Sell _ price1) _ _) (Node (Sell _ price2) _ _) = compare price1 price2

-- Define the OrderBook structure
data OrderBook = OrderBook
  { buyBid :: BuyBid,
    sellBid :: SellBid
  }

-- Implement the trade function
trade :: OrderBook -> [Bid] -> IO ()
trade _ [] = return ()
trade book (bid : rest) = do
  let (updatedBook, transaction) = processBid book bid
  when (isJust transaction) $ putStrLn (fromJust transaction)
  trade updatedBook rest

-- Define the processBid function to handle individual bids
processBid :: OrderBook -> Bid -> (OrderBook, Maybe String)
processBid book bid =
  case bid of
    Buy person price -> processBuy book person price
    Sell person price -> processSell book person price
    NewBuy person oldPrice newPrice -> processNewBuy book person oldPrice newPrice
    NewSell person oldPrice newPrice -> processNewSell book person oldPrice newPrice

-- Define functions to process individual bid types
processBuy :: OrderBook -> Person -> Price -> (OrderBook, Maybe String)
processBuy book@(OrderBook buy sell) person price =
  case minView sell of
    Nothing -> (book {buyBid = insert (Buy person price) (buyBid book)}, Nothing)
    Just (Sell seller askPrice, remainingSell) ->
      if askPrice <= price
        then (book {sellBid = remainingSell}, Just $ person ++ " buys a share from " ++ seller ++ " for " ++ show price ++ "kr")
        else (book {buyBid = insert (Buy person price) (buyBid book)}, Nothing)

processSell :: OrderBook -> Person -> Price -> (OrderBook, Maybe String)
processSell book@(OrderBook buy sell) person price =
  case minView buy of
    Nothing -> (book {sellBid = insert (Sell person price) (sellBid book)}, Nothing)
    Just (Buy buyer bidPrice, remainingBuy) ->
      if bidPrice >= price
        then (book {buyBid = remainingBuy}, Just $ buyer ++ " buys a share from " ++ person ++ " for " ++ show bidPrice ++ "kr")
        else (book {sellBid = insert (Sell person price) (sellBid book)}, Nothing)

processNewBuy :: OrderBook -> Person -> Price -> Price -> (OrderBook, Maybe String)
processNewBuy book@(OrderBook buy sell) person oldPrice newPrice =
  let updatedBuyBid = delete (Buy person oldPrice) (buyBid book)
   in (book {buyBid = insert (NewBuy person oldPrice newPrice) updatedBuyBid}, Nothing)

processNewSell :: OrderBook -> Person -> Price -> Price -> (OrderBook, Maybe String)
processNewSell book@(OrderBook buy sell) person oldPrice newPrice =
  let updatedSellBid = delete (Sell person oldPrice) (sellBid book)
   in (book {sellBid = insert (NewSell person oldPrice newPrice) updatedSellBid}, Nothing)

-- Main function to handle input and output
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: ./Lab2 <filename>"
    (filename:_) -> do
      contents <- readFile filename
      let bids = parseBids contents
          initialState = OrderBook {buyBid = Empty, sellBid = Empty}
      trade initialState bids
