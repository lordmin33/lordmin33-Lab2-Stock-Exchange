module PriorityQueue where

-- Add a  heap sort so that the skewheap is implemented correctly
data SkewHeap a = Empty
                | Node a (SkewHeap a) (SkewHeap a) deriving (Show, Eq)
-- test data
a = Node 2 Empty Empty
b = Node 6 Empty Empty
c = Node 3 (Node 6 (Node 12 Empty Empty) (Node 8 Empty Empty)) (Node 4 (Node 16 Empty Empty) (Node 15 Empty Empty))
d = Node 6 (Node 8 Empty Empty) Empty

-- used when inserting a new element to the heap
singleton :: Ord a => a -> SkewHeap a
singleton x = Node x Empty Empty

-- merges two skew heaps into one
merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge Empty Empty = Empty
merge h1 Empty  = h1
merge Empty h2  = h2
merge h1@(Node x1 l1 r1) h2@(Node x2 l2 r2)
    | x1 <= x2  = Node x1 (merge r1 h2) l1
    | otherwise = Node x2 (merge r2 h1) l2

-- inserts values into the heap using the singleton method
insert :: Ord a => a -> SkewHeap a -> SkewHeap a 
insert x sh = merge (singleton x) sh 

--extractMin :: Ord a => SkewHeap a -> SkewHeap a 
--extractMin Empty = Empty
--extractMin h@(Node x l r ) = merge l r

-- Extract the minimum value of the heap and merge the rest  
extractMin :: Ord a => SkewHeap a -> Maybe (a, SkewHeap a)
extractMin Empty        = Nothing
extractMin (Node x l r) = Just (x, merge l r)

-- deletes the first instance of a specific value in the heap
delete :: Ord a => a -> SkewHeap a -> SkewHeap a 
delete _ Empty = Empty
delete y h@(Node x l r) 
    | y == x          = (merge l r)
    | y > x           = Node x (delete y l) (delete y r)
    |otherwise        = h

toSortedList :: Ord a => SkewHeap a -> [a]
toSortedList Empty          = []
toSortedList (Node x l r)   = x : toSortedList (merge l r)

fromSortedList :: Ord a => [a] -> SkewHeap a
fromSortedList [] = Empty
fromSortedList xs = foldr (merge . singleton) Empty xs

find :: Ord a => a -> SkewHeap a -> Bool
find _ Empty = False
find x (Node y l r)
    | x == y    = True
    | x > y     = find x l || find x r
    | otherwise = False


update :: Ord a => a -> a -> SkewHeap a -> SkewHeap a
update _ _ Empty        = Empty
update x y sh 
    | delete x sh == sh = sh 
    | otherwise         = insert y (delete x sh)  
