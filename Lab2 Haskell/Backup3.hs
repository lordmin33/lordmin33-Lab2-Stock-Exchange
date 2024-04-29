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

type BuyBid  = MaxHeap Bid
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
  let initialState = OrderBook { buyBid = EmptyMax, sellBid = Empty }
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
  putStr "Sellers: " >> printBids (sellBid finalOrderBook)
  putStr "Buyers: " >> printBidsM (buyBid finalOrderBook)

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

processBuys :: OrderBook -> Bid ->  [String] -> (OrderBook, [String])
--processBuys book (Buy _ 0) xs = (book, xs)  -- Skip processing if bid price is 0
processBuys book@(OrderBook buy sell) bid@(Buy person price) xs =
  case compareM bid sell of
    Nothing -> if price > 0  -- only add values larger than
               then 
                let updatedBook = book {buyBid = insertM (Buy person price) (buyBid book)}
                  in (updatedBook, xs)
               else (book, xs)
    Just (Sell seller askPrice) ->
      if askPrice <= price  -- Trade occurs if buy price is greater than or equal to sell price
      then 
        let updatedBook =  book {sellBid = delete (Sell seller askPrice) (sellBid book)}
          in  (updatedBook, xs ++ [(show person ++ " buys from " ++ show seller ++ " for " ++ show price ++ "kr")])
      else (book, xs)

processSells :: OrderBook -> Bid -> [String] -> (OrderBook, [String])
--processSells book (Sell _ 0) xs = (book, xs)  -- Skip processing if bid price is 0
processSells book@(OrderBook buy sell) bid@(Sell person price) xs =
  case compare' bid buy of
    Nothing -> if price > 0  -- Only add non-zero bids to the order book
               then let updatedBook = book {sellBid = insert (Sell person price) (sellBid book)}
                in (updatedBook, xs)
               else (book, xs)
    Just (Buy buyer buyPrice) ->
      if price <= buyPrice  -- Trade occurs if sell price is less than or equal to buy price
      then 
        let updatedBook = book {buyBid = deleteM (Buy buyer buyPrice) (buyBid book)}
        in  (updatedBook, xs ++ [(show buyer ++ " buys from " ++ show person ++ " for " ++ show price ++ "kr")]) -- book {buyBid = insert (Buy buyer (buyPrice - price))}
      else (book, xs)

processNewBuy :: OrderBook -> Bid -> [String] -> (OrderBook, [String])
processNewBuy book@(OrderBook buy sell) bid@(NewBuy person oldPrice newPrice) xs = 
 let updatedBuyBid = deleteM (Buy person oldPrice) (buyBid book)
   in (processBuys (OrderBook updatedBuyBid sell) (Buy person newPrice) xs)

processNewSell :: OrderBook -> Bid -> [String] -> (OrderBook, [String])
processNewSell book@(OrderBook buy sell) bid@(NewSell person oldPrice newPrice) xs =
  let updatedSellBid = delete (Sell person oldPrice) (sellBid book)
    in processSells (OrderBook buy updatedSellBid) (Sell person newPrice) xs
      --updatedBook = OrderBook buy (insert (Sell person newPrice) updatedSellBid)
  --in (updatedBook, xs)
 

compareM :: Bid -> SkewHeap Bid -> Maybe Bid -- Buy Bid
compareM _ Empty = Nothing  -- If the heap is empty, return Nothing  
compareM (Buy buyer buyPrice)  h@(Node x@(Sell seller sellPrice) l r) 
  | sellPrice <= buyPrice = Just x 
  | otherwise = case (compareM (Buy buyer buyPrice) l) of
                    Just match -> Just match
                    Nothing -> compareM (Buy buyer buyPrice) r 


compare' :: Bid -> MaxHeap Bid -> Maybe Bid -- Sell Bid
compare' _ EmptyMax = Nothing  -- If the heap is empty, return Nothing 
compare' (Sell seller sellPrice) h@(MaxNode x@(Buy buyer buyPrice) l r)
  | sellPrice <= buyPrice = Just x  -- If the sell price is less than or equal to the buy price, return the buy bid
  | otherwise = case (compare' (Sell seller sellPrice) l) of  -- Otherwise, recursively search in the left subtree
                    Just match -> Just match
                    Nothing -> compare' (Sell seller sellPrice) r  -- If not found in the left subtree, recursively search in the right subtree


largestPrice :: SkewHeap Bid -> Price
largestPrice h@(Node x@(Buy buyer buyPrice) l r) =
  case (findLargest h) of 
    Nothing -> 0
    Just x -> buyPrice

printBidsM :: MaxHeap Bid -> IO ()
printBidsM sh =  putStrLn(listToString (toSortedListM sh))

printBids :: SkewHeap Bid -> IO ()
printBids sh =  putStrLn(listToString ((toSortedList sh)))

listToString :: Show a => [a] -> String
listToString xs = concat $ intersperse ", " (map show xs) 

t1 = trade [(Buy "a" 2),(Buy "j" 6),(Buy "g" 4),(Sell "b" 6),(Buy "c" 6),(Sell "y" 17),(Sell "d" 6), (Buy "oo" 18), (Buy "gg" 0)] -- nah it dosn't make sense, i = 0,2 försvinner av någon anledning
t2 = trade [(Buy "a" 2),(Buy "j" 6),(Buy "g" 4),(Sell "b" 6),(Buy "c" 6),(Sell "y" 17),(Sell "d" 6),(Buy "c" 6),(Sell "k" 2),(Sell "d8" 6),(Buy "c" 6),(Sell "k" 2),(Sell "o" 6),(Buy "k2" 6),(Sell "k1" 2),(Sell "ä" 88),(Buy "åå" 76),(Sell "å" 2), (Sell "b" 7), (Sell "B" 7), (Sell "b" 7), (Buy "a" 2)]
t3 = trade [(Buy "a" 2),(Buy "j" 6),(Buy "g" 4),(Sell "b" 6),(Buy "c" 6),(Sell "y" 17),(Sell "d" 6),(Buy "c" 6),(Sell "k" 2),(Sell "d8" 6)]
t4 = trade [(Sell "a" 2),(Sell "j" 6),(Sell "g" 4),(Sell "b" 6),(Sell "c" 6),(Sell "y" 17),(Sell "d" 6),(Buy "c" 6),(Sell "k" 2),(Sell "d8" 6),(Sell "g" 4),(Sell "b" 6),(Sell "c" 6),(Sell "y" 17),(Sell "d" 6),(Sell "c" 6),(Sell "k" 2),(Sell "d8" 6),(Sell "j" 6),(Sell "g" 4),(Sell "b" 6),(Sell "c" 6),(Sell "y" 17),(Sell "d" 6),(Sell "c" 6),(Sell "k" 2),(Sell "d8" 6),(Sell "g" 4),(Sell "b" 6),(Sell "c" 6),(Sell "y" 17),(Sell "d" 6),(Sell "c" 6),(Sell "k" 2),(Buy "gh" 6)]
t5 = trade [(Buy "a" 2),(Buy "j" 6),(Buy "g" 4),(NewBuy "j" 6 7),(Buy "c" 6),(Buy "y" 17),(Buy "d" 6),(Buy "c" 6),(Buy "k" 2),(Buy "d8" 6),(Buy "c" 6),(Buy "k" 2),(Buy "o" 6),(Buy "k2" 6),(Buy "k1" 2),(Buy "ä" 88),(Buy "åå" 76),(Buy "å" 2), (Buy "b" 7), (Buy "B" 7), (Buy "b" 7), (Buy "a" 2)]
t6 = trade [(Buy "a" 2),(Buy "b" 6),(Buy "c" 4),(Sell "d" 6),(Buy "e" 6),(Sell "f" 17),(Sell "g" 6),(Buy "h" 6),(Sell "i" 2),(Sell "j" 6)]
t7 = trade [(Sell "a" 2),(Sell "b" 6),(Sell "c" 4),(Sell "d" 6),(Sell "e" 6),(Sell "f" 17),(Sell "g" 6),(Sell "h" 6),(Sell "i" 2),(Sell "j" 6), (Buy "k" 177)]
t8 = trade [(Sell "a" 1),(Sell "b" 1),(Sell "c" 1),(Sell "d" 1),(Sell "e" 1),(Sell "f" 1),(Sell "g" 1),(Sell "h" 1),(Sell "i" 1),(Sell "j" 1), (Buy "k" 1)] --10 sell, 1 buy
t9 = trade [(Buy "a" 2), (Buy "j" 6), (Buy "g" 4),(Sell "b" 6)]
t10 = trade [(Buy "a" 2),(Buy "b" 3),(Buy "c" 9),(Sell "d" 5),(Sell "d" 1),(Sell "d" 1),(Sell "d" 1),(Sell "d" 1),(Sell "d" 1)]
t11 = trade [(Sell "p0" 65536), (Buy "p1" 32768), (Buy "p2" 16384), (NewBuy "p1" 32768 24576), (NewSell "p0" 65536 32768), (Sell "p5" 65536)]
t12 = trade [(Sell "p0" 65536), (Buy "p1" 32768), (Buy "p2" 16384), (NewBuy "p1" 32768 24576), (NewSell "p0" 65536 32768), (NewSell "p0" 32768 16384)]