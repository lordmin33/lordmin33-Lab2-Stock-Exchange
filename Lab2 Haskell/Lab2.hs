module Lab2 where

import Data.List hiding (insert, delete, find)
import Control.Applicative
import System.Environment
import System.IO
import PriorityQueue
--import TheOrderBook

-- | Bids.
data Bid
  = Buy Person Price           -- Person offers to buy share
  | Sell Person Price          -- Person offers to sell share
  | NewBuy Person Price Price  -- Person changes buy bid
  | NewSell Person Price Price -- Person changes sell bid

type Person = String
type Price = Integer

type BuyBid = Bid 
type SellBid = Bid 

type BuyQueue  = SkewHeap BuyBid
type SellQueue = SkewHeap SellBid

data OrderBook = OrderBook { 
  buyQueue  :: BuyQueue, --somehow make the buyQueue into a max Skewheap
  sellQueue :: SellQueue  -- that way we don't need to use compare' anymore since 
  }                     -- we will compare min from sellQueue and max from buyQueue by using extracteMin

-- Define Ord instance for Bid
{- 
instance Ord Bid where
  compare (Buy _ price1) (Buy _ price2) = compare price2 price1  -- Reverse order for max heap
  compare (Sell _ price1) (Sell _ price2) = compare price1 price2
  compare _ _ = EQ  -- Just for completeness, other cases don't matter for OrderBook
-}
-- Define custom Ord instances for BuyBid and SellBid
instance Ord BuyBid where
  compare (Buy _ price1) (Buy _ price2) = compare price1 price2  -- Normal ordering for buy bids
  compare _ _ = EQ  -- Just for completeness, other cases don't matter for OrderBook

instance Ord SellBid where
  compare (Sell _ price1) (Sell _ price2) = compare price2 price1  -- Reverse ordering for sell bids
  compare _ _ = EQ  -- Just for completeness, other cases don't matter for OrderBook


-- Example function to insert a bid into the order book
insertBid :: Bid -> OrderBook -> OrderBook
insertBid bid (OrderBook buy sell) =
  case bid of
    Buy _ _ -> OrderBook (insert bid buy) sell
    Sell _ _ -> OrderBook buy (insert bid sell)
    _ -> error "Unsupported bid type"


instance Show Bid where
  show (Buy person price)       = person ++ " " ++ show price
  show (Sell person price)      = person ++ " " ++ show price

instance Eq Bid where
    (Buy s1 i1) == (Buy s2 i2) = s1 == s2 && i1 == i2
    (Sell s1 i1) == (Sell s2 i2) = s1 == s2 && i1 == i2
    _ == _ = False

-- | Parses a bid. Incorrectly formatted bids are returned verbatim
-- (tagged with 'Left').

parseBid :: String -> Either String Bid
parseBid s = case words s of
  name : kind : prices ->
    case (kind, mapM readInteger prices) of
      ("K",  Just [price])              -> Right (Buy name price)
      ("S",  Just [price])              -> Right (Sell name price)
      ("NK", Just [oldPrice, newPrice]) -> Right (NewBuy name oldPrice newPrice)
      ("NS", Just [oldPrice, newPrice]) -> Right (NewSell name oldPrice newPrice)
      _ -> Left s
  _ -> Left s
  where
  readInteger :: String -> Maybe Integer
  readInteger s = case filter (null . snd) $ reads s of
    [(x, _)] -> Just x
    _        -> Nothing

-- | Parses a sequence of bids. Correctly formatted bids are returned
-- (in the order encountered), and an error message is printed for
-- each incorrectly formatted bid.

parseBids :: String -> IO [Bid]
parseBids s = concat <$> mapM (check . parseBid) (lines s)
  where
  check (Left bid)  = do
    hPutStrLn stderr $ "Malformed bid: " ++ bid
    return []
  check (Right bid) = return [bid]

-- | The main function of the program.

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> process stdin
    [f] -> process =<< openFile f ReadMode
    _   -> hPutStr stderr $ unlines
      [ "Usage: ./Lab2 [<file>]"
      , "If no file is given, then input is read from standard input."
      ]
  where
  process h = trade =<< parseBids =<< hGetContents h

-- | The core of the program. Takes a list of bids and executes them.

trade :: [Bid] -> IO()
trade bids = do
  let initialState = OrderBook {buyQueue = Empty, sellQueue = Empty }
  orderBook initialState bids

-- Maybe put everything in the book and then check if there exist a buyer for the seller? probably somewhat hard to implement
orderBook :: OrderBook -> [Bid] -> IO()
-- orderBook book [] = book
orderBook book bids = do
  let initialList = [] 
  let (finalOrderBook, xs) = processBids book bids initialList
  
  -- Print out the list of strings
  mapM_ putStrLn xs

  putStrLn "Order book:"
  putStr "Sellers: " >> printBids (sellQueue finalOrderBook)
  putStr "Buyers: " >> printBids (buyQueue finalOrderBook)

processBids :: OrderBook -> [Bid] ->  [String] -> (OrderBook, [String]) --O(n), goes until the list is empty
processBids book [] xs = (book, xs) 
processBids book (bid:rest) xs = case bid of 
    Buy person price                  -> let (oBook, ys) = (processBuys book bid xs)
      in processBids oBook rest ys
    Sell person price                 -> let (oBook, ys) = (processSells book bid xs)
      in processBids oBook rest ys
    NewBuy person oldPrice newPrice   -> let (oBook, ys) = (processNewBuy book bid xs)
      in processBids oBook rest ys
    NewSell person oldPrice newPrice  -> let (oBook, ys) = (processNewSell book bid xs)
      in processBids oBook rest ys

processBuys :: OrderBook -> Bid -> [String] -> (OrderBook, [String])
processBuys book bid@(Buy person 0) xs = (book,xs)
processBuys book@(OrderBook buy sell) bid@(Buy person price) xs =
  case extractMin sell of
    Nothing -> (book, xs)
    Just s@((Sell seller askPrice), updatedSellQueue) -> 
      if askPrice <= price
        then --let updatedBook = book {sellQueue = updatedSellQueue}
              ((OrderBook buy updatedSellQueue), xs ++ [(show person ++ " buys from " ++ show seller ++ " for " ++ show price)])
      else   --let updatedBook' = book {sellQueue = insert s updatedSellQueue}
              ((OrderBook buy (insert s updatedSellQueue)), xs ++ [(show person ++ " buys from " ++ show buyer ++ " for " ++ show price)])

{-
processBuys book@(OrderBook buy sell) bid@(Buy person price) xs =
  case compare' bid sell of
    Nothing -> if price > 0
               then 
                let updatedBook = book {buyQueue = insert (Buy person price) (buyQueue book)}
                  in (updatedBook, xs)
               else (book, xs)
    Just (Sell seller askPrice) ->
      if askPrice <= price
      then 
        let updatedBook =  book {sellQueue = delete (Sell seller askPrice) (sellQueue book)}
          in  (updatedBook, xs ++ [(show person ++ " buys from " ++ show seller ++ " for " ++ show price)])
      else (book, xs)
-}

processSells :: OrderBook -> Bid -> [String] -> (OrderBook, [String])
processSells book bid@(Sell person 0) xs = (book, xs)
processSells book@(OrderBook buy sell) bid@(Buy person price) xs =
  case extractMin buy of
    Nothing -> (book, xs)
    Just (b@(Buy buyer askPrice), updatedBuyQueue) -> 
      if askprice <= price 
        then --let updatedBook = book {buyQueue = updatedBuyQueue}
              ((OrderBook updatedBuyQueue sell), xs ++ [(show person ++ " buys from " ++ show buyer ++ " for " ++ show price)])
      else  --let updatedBook' = book {buyQueue = insert b updatedSellQueue}
               (OrderBook (insert b updatedSellQueue) sell, xs ++ [(show person ++ " buys from " ++ show buyer ++ " for " ++ show price)])

{-
processSells book@(OrderBook buy sell) bid@(Sell person price) xs =
  case compare' bid buy of
    Nothing -> if price > 0  -- Only add non-zero bids to the order book
               then let updatedBook = book {sellQueue = insert (Sell person price) (sellQueue book)}
                in (updatedBook, xs)
               else (book, xs)
    Just (Buy buyer buyPrice) ->
      if price <= buyPrice  -- Trade occurs if buy price is greater than or equal to sell price
      then 
        let updatedBook = book {buyQueue = delete (Buy buyer buyPrice) (buyQueue book)}
        in  (updatedBook, xs ++ [(show buyer ++ " buys from " ++ show person ++ " for " ++ show price)]) -- book {buyBid = insert (Buy buyer (buyPrice - price))}
      else (book, xs) -}

processNewBuy :: OrderBook -> Bid -> [String] -> (OrderBook, [String])
processNewBuy book@(OrderBook buy sell) bid@(NewBuy person oldPrice newPrice) xs = 
 let updatedBuyBid = delete (Buy person oldPrice) (buyQueue book)
   in (processBuys (OrderBook updatedBuyBid sell) (Buy person newPrice) xs)

processNewSell :: OrderBook -> Bid ->  [String] -> (OrderBook, [String])
processNewSell book@(OrderBook buy sell) bid@(NewSell person oldPrice newPrice) xs =
  let updatedSellBid = delete (Sell person oldPrice) (sellQueue book)
   in (processSells (OrderBook buy updatedSellBid) (Sell person newPrice) xs) 

{-
compare' :: Bid -> SkewHeap Bid -> Maybe Bid
compare' _ Empty = Nothing  -- If the heap is empty, return Nothing 
compare' (Sell seller sellPrice) y@(Node x@(Buy buyer buyPrice) l r)
  | sellPrice <= largestPrice y = Just x  -- If the sell price is less than or equal to the buy price, return the buy bid
  | otherwise = case (compare' (Sell seller sellPrice) l) of  -- Otherwise, recursively search in the left subtree
                    Just match -> Just match
                    Nothing -> compare' (Sell seller sellPrice) r  -- If not found in the left subtree, recursively search in the right subtree
compare' (Buy buyer buyPrice2)  h@(Node x@(Sell seller2 sellPrice2) l r) 
  | sellPrice2 <= buyPrice2 = Just x 
  | otherwise = case (compare' (Buy buyer buyPrice2) l) of
                    Just match -> Just match
                    Nothing -> compare' (Buy buyer buyPrice2) r 
    where 
      maxprice = undefined -- largestPrice y
-}

printBids :: SkewHeap Bid -> IO ()
printBids sh =  putStrLn(listToString (toSortedList sh))

listToString :: Show a => [a] -> String
listToString xs = concat $ intersperse ", " (map show xs) 