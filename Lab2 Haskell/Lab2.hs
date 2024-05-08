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

type BuyBid =  Bid
type SellBid = Bid

type BuyQueue  = SkewHeap BuyBid
type SellQueue = SkewHeap SellBid

data OrderBook = OrderBook { 
  buyQueue  :: BuyQueue,
  sellQueue :: SellQueue  
}                     
{-
--newtype BuyQueue a = BuyQueue (SkewHeap a) deriving (Eq, Show)


-- Define the Ord instance for FlipOrd
instance Ord a => Ord (BuyQueue) where
    compare = flip compare

-- Define the Bounded instance for FlipOrd
instance Bounded a => Bounded (BuyQueue) where
    minBound = BuyQueue maxBound
    maxBound = BuyQueue minBound
-}
{-
instance Ord BuyBid where
  compare (Buy _ price1) (Buy _ price2) = compare price2 price1
  compare _ _ = EQ  -- Just for completeness, other cases don't matter for OrderBook

instance Ord SellBid where
  compare (Sell _ price1) (Sell _ price2) = compare price1 price2
  compare _ _ = EQ  -- Just for completeness, other cases don't matter for OrderBook

instance Ord (SkewHeap BuyBid) where
  compare = compare

instance Bounded (SkewHeap BuyBid) where
  minBound = Empty
  maxBound = undefined  -- Not sure what maxBound should be for a skew heap
-}
-- Similar instances for SellBid and SkewHeap SellBid


-- Define Ord instance for Bid

instance Ord Bid where
  compare (Buy _ price1) (Buy _ price2) = compare price2 price1  --Reverse order for max heap
  compare (Sell _ price1) (Sell _ price2) = compare price1 price2
  compare _ _ = EQ  -- Just for completeness, other cases don't matter for OrderBook

{-
-- Define custom Ord instances for BuyBid and SellBid
instance Ord BuyBid where
  compare (Buy _ price1) (Buy _ price2) = compare price2 price1  --Reverse ordering for buy bids
  compare _ _ = EQ  -- Just for completeness, other cases don't matter for OrderBook

instance Ord SellBid where
  compare (Sell _ price1) (Sell _ price2) = compare price1 price2  -- Normal ordering for sell bids
  compare _ _ = EQ  -- Just for completeness, other cases don't matter for OrderBook
-}

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
  let initialState = OrderBook emptyHeap emptyHeap
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

processBids :: OrderBook -> [Bid] ->  [String] -> (OrderBook, [String]) -- process the bids until the list is empty
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
    Nothing -> (insertBid bid book, xs)
    Just (s@(Sell seller askPrice), updatedSellQueue) -> 
      if askPrice <= price
        then --let updatedBook = book {sellQueue = updatedSellQueue}
              ((OrderBook buy updatedSellQueue), xs ++ [(show person ++ " buys from " ++ show seller ++ " for " ++ show price)])
      else   --let updatedBook' = book {sellQueue = insert s updatedSellQueue}
            let updatedBook = insertBid bid $ insertBid s (OrderBook buy updatedSellQueue)
              in (updatedBook, xs)
              --((OrderBook buy (insert s updatedSellQueue)), xs)

processSells :: OrderBook -> Bid -> [String] -> (OrderBook, [String])
processSells book bid@(Sell person 0) xs = (book, xs)
processSells book@(OrderBook buy sell) bid@(Sell person askprice) xs =
  case extractMin buy of
    Nothing -> (insertBid bid book, xs)
    Just (b@(Buy buyer price), updatedBuyQueue) -> 
      if askprice <= price 
        then --let updatedBook = book {buyQueue = updatedBuyQueue}
              ((OrderBook updatedBuyQueue sell), xs ++ [(show buyer ++ " buys from " ++ show person ++ " for " ++ show price)])
      else  --let updatedBook' = book {buyQueue = insert b updatedBuyQueue}
              let updatedBook = insertBid bid $ insertBid b (OrderBook updatedBuyQueue sell)
               in (updatedBook, xs)
                --((OrderBook (insert b updatedBuyQueue)) sell, xs) 

processNewBuy :: OrderBook -> Bid -> [String] -> (OrderBook, [String])
processNewBuy book@(OrderBook buy sell) bid@(NewBuy person oldPrice newPrice) xs = 
 let updatedBuyBid = delete (Buy person oldPrice) buy
   in (processBuys (OrderBook updatedBuyBid sell) (Buy person newPrice) xs)

processNewSell :: OrderBook -> Bid ->  [String] -> (OrderBook, [String])
processNewSell book@(OrderBook buy sell) bid@(NewSell person oldPrice newPrice) xs =
  let updatedSellBid = delete (Sell person oldPrice) sell
   in (processSells (OrderBook buy updatedSellBid) (Sell person newPrice) xs) 

-- insert bid into the OrderBook 
insertBid :: Bid -> OrderBook -> OrderBook
insertBid bid (OrderBook buy sell) =
  case bid of
    Buy _ _ -> OrderBook (insert bid buy) sell
    Sell _ _ -> OrderBook buy (insert bid sell)
    _ -> error "Unsupported bid type"

printBids :: SkewHeap Bid -> IO ()
printBids sh =  putStrLn(listToString (toSortedList sh))

printBids' :: MaxHeap Bid -> IO ()
printBids' sh =  putStrLn(listToString (toSortedListM sh))

listToString :: Show a => [a] -> String
listToString xs = concat $ intersperse ", " (map show xs) 

t11 = trade [(Sell "p0" 65536), (Buy "p1" 32768), (Buy "p2" 16384), (NewBuy "p1" 32768 24576), (NewSell "p0" 65536 32768), (Sell "p5" 65536)]
t12 = trade [(Sell "p0" 65536), (Buy "p1" 32768), (Buy "p2" 16384), (NewBuy "p1" 32768 24576), (NewSell "p0" 65536 32768), (NewSell "p0" 32768 16384)]
