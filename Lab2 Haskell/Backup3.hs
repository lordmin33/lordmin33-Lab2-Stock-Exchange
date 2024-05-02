-- Lab2.hs

module Lab2 where

import Data.List hiding (insert, delete, find)
import Control.Applicative
import System.Environment
import System.IO
import PriorityQueue

data Bid
  = Buy Person Price
  | Sell Person Price
  | NewBuy Person Price Price
  | NewSell Person Price Price

type Person = String
type Price = Integer

type BuyBid  = SkewHeap BuyBidItem
type SellBid = SkewHeap SellBidItem

data BuyBidItem = BuyBidItem Person Price deriving (Show, Eq, Ord)
data SellBidItem = SellBidItem Person Price deriving (Show, Eq, Ord)

data OrderBook = OrderBook {
  buyBid  :: BuyBid,
  sellBid :: SellBid
}

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

parseBids :: String -> IO [Bid]
parseBids s = concat <$> mapM (check . parseBid) (lines s)
  where
  check (Left bid)  = do
    hPutStrLn stderr $ "Malformed bid: " ++ bid
    return []
  check (Right bid) = return [bid]

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

trade :: [Bid] -> IO()
trade bids = do
  let initialState = OrderBook { buyBid = Empty, sellBid = Empty }
  orderBook initialState bids

orderBook :: OrderBook -> [Bid] -> IO()
orderBook book bids = do
  let (finalOrderBook, transactions) = processBids book bids []
  mapM_ putStrLn transactions
  putStrLn "Order book:"
  putStr "Sellers: " >> printBids (sellBid finalOrderBook)
  putStr "Buyers: " >> printBids (buyBid finalOrderBook)

processBids :: OrderBook -> [Bid] -> [String] -> (OrderBook, [String])
processBids book [] xs = (book, xs)
processBids book (bid:rest) xs = case bid of
  Buy person price          -> let (oBook, ys) = processBuys book bid xs
                               in processBids oBook rest ys
  Sell person price         -> let (oBook, ys) = processSells book bid xs
                               in processBids oBook rest ys
  NewBuy person old new     -> let (oBook, ys) = processNewBuy book bid xs
                               in processBids oBook rest ys
  NewSell person old new    -> let (oBook, ys) = processNewSell book bid xs
                               in processBids oBook rest ys

processBuys :: OrderBook -> Bid -> [String] -> (OrderBook, [String])
processBuys book@(OrderBook buy sell) bid@(Buy person price) xs =
  let updatedBuyBid = insert (BuyBidItem person price) buy
  in case extractMin sell of
    Nothing              -> (OrderBook updatedBuyBid sell, xs)
    Just (bestSell, restSell) -> case bestSell of
      SellBidItem s p -> if p <= price
                         then let updatedSellBid = restSell
                               in (OrderBook updatedBuyBid updatedSellBid, xs ++ [show person ++ " buys from " ++ show s ++ " for " ++ show p])
                         else (book, xs)

processSells :: OrderBook -> Bid -> [String] -> (OrderBook, [String])
processSells book@(OrderBook buy sell) bid@(Sell person price) xs =
  let updatedSellBid = insert (SellBidItem person price) sell
  in case extractMin buy of
    Nothing             -> (OrderBook buy updatedSellBid, xs)
    Just (bestBuy, restBuy) -> case bestBuy of
      BuyBidItem b p -> if p >= price
                        then let updatedBuyBid = restBuy
                             in (OrderBook updatedBuyBid updatedSellBid, xs ++ [show b ++ " buys from " ++ show person ++ " for " ++ show price])
                        else (book, xs)


processNewBuy :: OrderBook -> Bid -> [String] -> (OrderBook, [String])
processNewBuy book@(OrderBook buy sell) bid@(NewBuy person old new) xs =
  let updatedBuyBid = delete (BuyBidItem person old) buy
  in processBuys (OrderBook (insert (BuyBidItem person new) updatedBuyBid) sell) (Buy person new) xs

processNewSell :: OrderBook -> Bid -> [String] -> (OrderBook, [String])
processNewSell book@(OrderBook buy sell) bid@(NewSell person old new) xs =
  let updatedSellBid = delete (SellBidItem person old) sell
  in processSells (OrderBook buy (insert (SellBidItem person new) updatedSellBid)) (Sell person new) xs

printBids :: (Show a, Ord a) => SkewHeap a -> IO ()
printBids sh = putStrLn (listToString (toSortedList sh))


listToString :: Show a => [a] -> String
listToString xs = intercalate ", " (map show xs)


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
-- HEAD
t11 = trade [(Sell "p0" 65536), (Buy "p1" 32768), (Buy "p2" 16384), (NewBuy "p1" 32768 24576), (NewSell "p0" 65536 32768), (Sell "p5" 65536)]
t12 = trade [(Sell "p0" 65536), (Buy "p1" 32768), (Buy "p2" 16384), (NewBuy "p1" 32768 24576), (NewSell "p0" 65536 32768), (NewSell "p0" 32768 16384)]