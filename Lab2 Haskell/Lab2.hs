module Lab2 where

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
  deriving Show

type Person = String
type Price = Integer

data BuyBid = BuyBid {
  buyBi :: Buy
}

data SellBid = SellBid {
  sellBi :: Sell
}

type BuyOrder  = SkewHeap BuyBid
type SellOrder = SkewHeap SellBid

data OrderBook = OrderBook { buyBid :: BuyOrder,
                            sellBid ::  SellOrder }

--instance Ord BuyBid where
--    Compare BuyBid BuyBid = EQ
--    Compare BuyBid BuyBid = LT
--    Compare BuyBid BuyBid = LT



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

trade :: [Bid] -> IO ()
trade = undefined


helperFunction :: OrderBook -> [Bid] -> IO()
helperFunction = undefined