module PriorityQueue where

import Lab2

-- Add a  heap sort so that the skewheap is implemented correctly
data SkewHeap a = Empty
                | Node a (SkewHeap a) (SkewHeap a) deriving (Show, Eq)
-- test data
a = Node 2 Empty Empty
b = Node 6 Empty Empty
c = Node 3 (Node 6 (Node 8 Empty Empty) (Node 12 Empty Empty)) (Node 4 (Node 16 Empty Empty) (Node 17 Empty Empty))
d = Node 6 Empty (Node 8 Empty Empty)

-- should make sure value can't be changed
ivariant :: Ord a => a -> SkewHeap a 
ivariant a = undefined

-- used when inserting a new element to the heap
singleton :: Ord a => a -> SkewHeap a
singleton x = Node x Empty Empty

-- rank :: SkewHeap a -> a
-- rank Empty          = 0
-- rank (Node _ _ r )  = r + 1

-- merges two skew heaps into one
merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge h1 Empty = h1
merge Empty h2 = h2
merge h1@(Node x1 l1 r1) h2@(Node x2 l2 r2)
    | x1 <= x2  = Node x1 (merge r1 h2) l1
    | otherwise = Node x2 (merge r2 h1) l2

-- inserts values into the heap using the singleton method
insert :: Ord a => a -> SkewHeap a -> SkewHeap a 
insert x sh = merge (singleton x) sh 

extract_min :: Ord a => SkewHeap a -> SkewHeap a 
extract_min h@(Node x l r ) = merge l r

-- deletes the first instance of a specific value in the heap
delete :: Ord a => a -> SkewHeap a -> SkewHeap a 
delete _ Empty = Empty
delete y h@(Node x l r) 
    | y == x          = (merge l r)
    | y > x           = Node x (delete y l) (delete y r)
    |otherwise        = h

update :: Ord a => a -> a -> SkewHeap a -> SkewHeap a
update x y Empty = Empty
update x y sh 
    | delete x sh == sh = sh 
    | otherwise         = insert y (delete x sh) 
    