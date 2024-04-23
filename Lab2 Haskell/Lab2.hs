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

type BuyBid  = SkewHeap Bid
type SellBid = SkewHeap Bid

data OrderBook = OrderBook { 
  buyBid  :: BuyBid,
  sellBid :: SellBid 
  }

--instance Show Price where
--  show (Price p) = show p

instance Eq Bid where
    (Buy s1 i1) == (Buy s2 i2) = s1 == s2 && i1 == i2
    (Sell s1 i1) == (Sell s2 i2) = s1 == s2 && i1 == i2
    _ == _ = False

instance Show Bid where
  show (Buy person price)       = person ++ " " ++ show price
  show (Sell person price)      = person ++ " " ++ show price

instance Ord Bid where
  compare (Buy _ price1) (Buy _ price2) = compare price1 price2
  compare (Buy _ _) (Sell _ _) = LT
  compare (Buy _ _) (NewBuy _ _ _) = LT
  compare (Buy _ _) (NewSell _ _ _) = LT
  compare (Sell _ price1) (Sell _ price2) = compare price1 price2
  compare (Sell _ _) (NewBuy _ _ _) = LT
  compare (Sell _ _) (NewSell _ _ _) = LT
  compare (NewBuy _ _ _) (NewSell _ _ _) = EQ
  compare (NewBuy _ _ _) (Buy _ _) = GT
  compare (NewBuy _ _ _) (Sell _ _) = GT
  compare (NewSell _ _ _) (Buy _ _) = GT
  compare (NewSell _ _ _) (Sell _ _) = GT


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
  let initialState = OrderBook { buyBid = Empty, sellBid = Empty }
  orderBook initialState bids

-- Maybe put everything in the book and then check if there exist a buyer for the seller? probably somewhat hard to implement
orderBook :: OrderBook -> [Bid] -> IO()
--orderBook book [] = book
orderBook book bids = do
  let finalOrderBook = processBids book bids
  
  putStrLn "Order book:"
  putStr "Sellers: " >> printBids (sellBid finalOrderBook)
  putStr "Buyers: " >> printBids (buyBid finalOrderBook)

processBids :: OrderBook -> [Bid] -> OrderBook 
processBids book [] = book 
processBids book (bid:rest) = case bid of 
    Buy person price                  -> processBids (processBuys book bid ) rest
    Sell person price                 -> processBids (processSells book bid) rest 
    NewBuy person oldPrice newPrice   -> processBids (processNewBuy book bid) rest
    NewSell person oldPrice newPrice  -> processBids (processNewSell book bid) rest

t1 = trade [(Buy "a" 2),(Buy "j" 6),(Buy "g" 4),(Sell "b" 6),(Buy "c" 6),(Sell "y" 17),(Sell "d" 6), (Buy "oo" 18), (Buy "gg" 0)] -- nah it dosn't make sense, i = 0,2 försvinner av någon anledning
t2 = trade [(Buy "a" 2),(Buy "j" 6),(Buy "g" 4),(Sell "b" 6),(Buy "c" 6),(Sell "y" 17),(Sell "d" 6),(Buy "c" 6),(Sell "k" 2),(Sell "d8" 6),(Buy "c" 6),(Sell "k" 2),(Sell "o" 6),(Buy "k2" 6),(Sell "k1" 2),(Sell "ä" 88),(Buy "åå" 76),(Sell "å" 2), (Sell "b" 7), (Sell "B" 7), (Sell "b" 7), (Buy "a" 2)]
t3 = trade [(Buy "a" 2),(Buy "j" 6),(Buy "g" 4),(Sell "b" 6),(Buy "c" 6),(Sell "y" 17),(Sell "d" 6),(Buy "c" 6),(Sell "k" 2),(Sell "d8" 6)]
t4 = trade [(Sell "a" 2),(Sell "j" 6),(Sell "g" 4),(Sell "b" 6),(Sell "c" 6),(Sell "y" 17),(Sell "d" 6),(Buy "c" 6),(Sell "k" 2),(Sell "d8" 6),(Sell "g" 4),(Sell "b" 6),(Sell "c" 6),(Sell "y" 17),(Sell "d" 6),(Sell "c" 6),(Sell "k" 2),(Sell "d8" 6),(Sell "j" 6),(Sell "g" 4),(Sell "b" 6),(Sell "c" 6),(Sell "y" 17),(Sell "d" 6),(Sell "c" 6),(Sell "k" 2),(Sell "d8" 6),(Sell "g" 4),(Sell "b" 6),(Sell "c" 6),(Sell "y" 17),(Sell "d" 6),(Sell "c" 6),(Sell "k" 2),(Buy "gh" 6)]
t5 = trade [(Buy "a" 2),(Buy "j" 6),(Buy "g" 4),(NewBuy "j" 6 7),(Buy "c" 6),(Buy "y" 17),(Buy "d" 6),(Buy "c" 6),(Buy "k" 2),(Buy "d8" 6),(Buy "c" 6),(Buy "k" 2),(Buy "o" 6),(Buy "k2" 6),(Buy "k1" 2),(Buy "ä" 88),(Buy "åå" 76),(Buy "å" 2), (Buy "b" 7), (Buy "B" 7), (Buy "b" 7), (Buy "a" 2)]
t6 = trade [(Buy "a" 2),(Buy "b" 6),(Buy "c" 4),(Sell "d" 6),(Buy "e" 6),(Sell "f" 17),(Sell "g" 6),(Buy "h" 6),(Sell "i" 2),(Sell "j" 6)]
t7 = trade [(Sell "a" 2),(Sell "b" 6),(Sell "c" 4),(Sell "d" 6),(Sell "e" 6),(Sell "f" 17),(Sell "g" 6),(Sell "h" 6),(Sell "i" 2),(Sell "j" 6), (Buy "k" 177)]
t8 = trade [(Sell "a" 1),(Sell "b" 1),(Sell "c" 1),(Sell "d" 1),(Sell "e" 1),(Sell "f" 1),(Sell "g" 1),(Sell "h" 1),(Sell "i" 1),(Sell "j" 1), (Buy "k" 1)] --10 sell, 1 buy

processBuys :: OrderBook -> Bid -> OrderBook 
processBuys book (Buy _ 0) = book  -- Skip processing if bid price is 0
processBuys book@(OrderBook buy sell) bid@(Buy person price) =
  case compare' bid sell of
    Nothing -> if price > 0  -- Only add non-zero bids to the order book
               then book {buyBid = insert (Buy person price) (buyBid book)}
               else book
    Just (Sell seller askPrice) ->
      if price >= askPrice  -- Trade occurs if buy price is greater than or equal to sell price
      then 
        processBuys (book { sellBid = delete (Sell seller askPrice) (sellBid book) }) (Buy person (price - askPrice))
      else book

processSells :: OrderBook -> Bid -> OrderBook
processSells book (Sell _ 0) = book  -- Skip processing if bid price is 0
processSells book@(OrderBook buy sell) bid@(Sell person price) =
  case compare' bid buy of
    Nothing -> if price > 0  -- Only add non-zero bids to the order book
               then book {sellBid = insert (Sell person price) (sellBid book)}
               else book
    Just (Buy buyer buyPrice) ->
      if price <= buyPrice  -- Trade occurs if sell price is less than or equal to buy price
      then processSells (book { buyBid = delete (Buy buyer buyPrice) (buyBid book) }) (Sell person (buyPrice - price))
      else book



processNewBuy :: OrderBook -> Bid -> OrderBook
processNewBuy book@(OrderBook buy sell) bid@(NewBuy person oldPrice newPrice) = 
 let updatedBuyBid = delete (Buy person oldPrice) (buyBid book)
   in (processBuys (OrderBook updatedBuyBid sell) (Buy person newPrice))

processNewSell :: OrderBook -> Bid -> OrderBook
processNewSell book@(OrderBook buy sell) bid@(NewSell person oldPrice newPrice) =
  let updatedSellBid = delete (Sell person oldPrice) (sellBid book)
   in (processSells (OrderBook buy updatedSellBid) (Sell person newPrice))

compare' :: Bid -> SkewHeap Bid -> Maybe Bid
compare' _ Empty = Nothing  -- If the heap is empty, return Nothing
compare' (Sell seller sellPrice) (Node x@(Buy buyer buyPrice) l r)
  | sellPrice == buyPrice = Just x  -- If the sell price is less than or equal to the buy price, return the buy bid
  | otherwise = case (compare' (Sell seller sellPrice) l) of  -- Otherwise, recursively search in the left subtree
                    Just match -> Just match
                    Nothing -> compare' (Sell seller sellPrice) r  -- If not found in the left subtree, recursively search in the right subtree
compare' (Buy buyer buyPrice2)  h@(Node x@(Sell seller2 sellPrice2) l r) 
  | sellPrice2 == buyPrice2 = Just x  
  | otherwise = case (compare' (Buy buyer buyPrice2) l) of
                    Just match -> Just match
                    Nothing -> compare' (Buy buyer buyPrice2) r 

printBids :: SkewHeap Bid -> IO ()
printBids sh =  putStrLn(listToString (toSortedList sh))

listToString :: Show a => [a] -> String
listToString xs = concat $ intersperse ", " (map show xs) 