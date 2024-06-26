module Lab2 where

import Data.List hiding (insert, delete, find)
import Control.Applicative
import System.Environment
import System.IO
import PriorityQueue

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

-- Define Ord instance for Bid
instance Ord Bid where
  compare (Buy _ price1) (Buy _ price2) = compare price2 price1  --Reverse order for max SkewHeap
  compare (Sell _ price1) (Sell _ price2) = compare price1 price2
  compare _ _ = error "not allowed"  -- Just for completeness, other cases don't matter for OrderBook

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
  let initialState = OrderBook emptySkewHeap emptySkewHeap
  orderBook initialState bids

-- Maybe put everything in the book and then check if there exist a buyer for the seller? probably somewhat hard to implement
orderBook :: OrderBook -> [Bid] -> IO()
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

-- Could make a general function for processBuys and processSells by 
-- first adding the new bid to the heap then comparing maxBid form buy and minBid from sell
-- if price match then do the process that 
-- else insert back into the heaps
-- we did not have time to implement it 
processBuys :: OrderBook -> Bid -> [String] -> (OrderBook, [String])
processBuys book bid@(Buy person 0) xs = (book,xs)
processBuys book@(OrderBook buy sell) bid@(Buy person price) xs =
  case extractMin sell of
    Nothing -> (insertBid bid book, xs) -- insert bid if sell is empty
    Just (s@(Sell seller askPrice), updatedSellQueue) -> 
      if askPrice <= price -- compare the prices
        then  let transaction = [(show person ++ " buys from " ++ show seller ++ " for " ++ show price)]
            in ((OrderBook buy updatedSellQueue), xs ++ transaction) -- update the orderbook and the string list
      else   let updatedBook = insertBid bid $ insertBid s (OrderBook buy updatedSellQueue)-- insert sell min back into heap if prices is not matching 
              in (updatedBook, xs)                                                         -- insert bid in its corresponding heap 

processSells :: OrderBook -> Bid -> [String] -> (OrderBook, [String])
processSells book bid@(Sell person 0) xs = (book, xs)
processSells book@(OrderBook buy sell) bid@(Sell person askprice) xs =
  case extractMin buy of
    Nothing -> (insertBid bid book, xs) -- insert bid if buy is empty
    Just (b@(Buy buyer price), updatedBuyQueue) -> 
      if askprice <= price -- compare the prices
        then let transaction = [(show buyer ++ " buys from " ++ show person ++ " for " ++ show price)]
            in ((OrderBook updatedBuyQueue sell), xs ++ transaction)  -- update the orderbook and the string list
      else  let updatedBook = insertBid bid $ insertBid b (OrderBook updatedBuyQueue sell)--insert buy min back into heap if prices is not matching
               in (updatedBook, xs)                                                       -- insert bid in its corresponding heap 

processNewBuy :: OrderBook -> Bid -> [String] -> (OrderBook, [String])
processNewBuy book@(OrderBook buy sell) bid@(NewBuy person oldPrice newPrice) xs = 
 let updatedBuyBid = delete (Buy person oldPrice) buy
   in (processBuys (OrderBook updatedBuyBid sell) (Buy person newPrice) xs)

processNewSell :: OrderBook -> Bid ->  [String] -> (OrderBook, [String])
processNewSell book@(OrderBook buy sell) bid@(NewSell person oldPrice newPrice) xs =
  let updatedSellBid = delete (Sell person oldPrice) sell
   in (processSells (OrderBook buy updatedSellBid) (Sell person newPrice) xs) 

-- insert bid into the OrderBook in its corresponding heap
insertBid :: Bid -> OrderBook -> OrderBook
insertBid bid (OrderBook buy sell) =
  case bid of
    Buy _ _ -> OrderBook (insert bid buy) sell
    Sell _ _ -> OrderBook buy (insert bid sell)
    _ -> error "Unsupported bid type"

printBids :: SkewHeap Bid -> IO ()
printBids sh =  putStrLn(listToString (toSortedList sh))

listToString :: Show a => [a] -> String
listToString xs = concat $ intersperse ", " (map show xs) 