module Lab2 where

import Data.List hiding (insert, delete)
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
  deriving (Eq)

type Person = String
type Price = Integer

type BuyBid  = SkewHeap Bid
type SellBid = SkewHeap Bid

data OrderBook = OrderBook { 
  buyBid  :: BuyBid,
  sellBid :: SellBid 
  }

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

x = (Node (Buy "a" 2) (Node (Buy "b" 5) Empty Empty) (Node (Buy "c" 2) Empty Empty))

{-instance Ord BuyBid where
  compare Empty Empty = EQ
  compare Empty _ = LT
  compare _ Empty = GT
  compare (Node (Buy _ price1) _ _) (Node (Buy _ price2) _ _) = compare price1 price2


instance Ord SellBid where
  compare Empty Empty = EQ
  compare Empty _ = LT
  compare _ Empty = GT
  compare (Node (Sell _ price1) _ _) (Node (Sell _ price2) _ _) = compare price1 price2
-}  



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
  where 
    initialState' = OrderBook { buyBid = Empty, sellBid = Empty }

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

processBuys :: OrderBook -> Bid -> OrderBook
processBuys book@(OrderBook buy sell) bid@(Buy person price) = 
  --if price ==  (askprice) -- How to make it happen????? Need to check sellBid(a skeawHeap) but how????
  --        "Buyer ++ " buys from " ++ seller ++  " for " price" -- maybe print it out here?? 
  -- else
  book {buyBid = insert (Buy person price) (buyBid book)}


processSells :: OrderBook -> Bid -> OrderBook
processSells book@(OrderBook buy sell) bid@(Sell person price) = 
  book {sellBid = insert (Sell person price) (sellBid book)}
  
processNewBuy :: OrderBook -> Bid -> OrderBook
processNewBuy book@(OrderBook buy sell) bid@(NewBuy person oldPrice newPrice) = 
 let updatedBuyBid = delete (Buy person oldPrice) (buyBid book)
   in (processBuys (OrderBook updatedBuyBid sell) (Buy person newPrice))

processNewSell :: OrderBook -> Bid -> OrderBook
processNewSell book@(OrderBook buy sell) bid@(NewSell person oldPrice newPrice) =
  let updatedSellBid = delete (Sell person oldPrice) (sellBid book)
   in (processSells (OrderBook buy updatedSellBid) (Sell person newPrice))


printBids :: SkewHeap Bid -> IO ()
printBids sh =  putStrLn(listToString (toSortedList sh))

listToString :: Show a => [a] -> String
listToString xs = concat $ intersperse ", " (map show xs) 