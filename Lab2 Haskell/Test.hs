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
  deriving (Show, Eq)

instance Ord Bid where
  compare (Buy _ p1) (Buy _ p2) = compare p1 p2
  compare (Sell _ p1) (Sell _ p2) = compare p1 p2
  compare (NewBuy _ p1 _) (NewBuy _ p2 _) = compare p1 p2
  compare (NewSell _ p1 _) (NewSell _ p2 _) = compare p1 p2
  compare (Buy _ _) _ = GT
  compare _ (Buy _ _) = LT
  compare (Sell _ _) _ = GT
  compare _ (Sell _ _) = LT

type Person = String
type Price = Integer

type BuyBid = Bid 
type SellBid = Bid 

type BuyQueue  = SkewHeap BuyBid
type SellQueue = SkewHeap SellBid

data OrderBook = OrderBook { 
  buyQueue  :: BuyQueue,
  sellQueue :: SellQueue 
  }

i :: IO ()
i = do
    let t1 =  [(Sell "p0" 65536), (Buy "p1" 32768), (Buy "p2" 16384), (Sell "p5" 65536), (Sell "p0" 2), (Buy "p1" 10000), (Buy "p2" 2), (Sell "p5" 10000)]
        initialState = OrderBook { buyQueue = Empty, sellQueue = Empty }
        finalState = foldr i' initialState t1
    printOrderBook finalState

i' :: Bid -> OrderBook -> OrderBook
i' x ob = case x of
    Buy _ _    -> ob { buyQueue = insert x (buyQueue ob) }
    Sell _ _   -> ob { sellQueue = insert x (sellQueue ob) }
    NewBuy _ _ _ -> ob { buyQueue = insert x (buyQueue ob) }
    NewSell _ _ _-> ob { sellQueue = insert x (sellQueue ob) }

printOrderBook :: OrderBook -> IO ()
printOrderBook ob = do
    putStr "Buy Queue:" >> printBids (buyQueue ob)
    putStr "Sell Queue:" >> printBids (sellQueue ob)

printBids :: (Show a, Ord a) => SkewHeap a -> IO ()
printBids sh = putStrLn (listToString (toSortedList sh))

listToString :: Show a => [a] -> String
listToString xs = concat $ intersperse ", " (map show xs)

main :: IO ()
main = i
