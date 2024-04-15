module PriorityQueue where

import Lab2


data SkewHeap a = Empty
                | Node a (SkewHeap a) (SkewHeap a) deriving (Show, Eq)
-- test data
a = Node 2 Empty Empty
b = Node 6 Empty Empty
c = Node 3 (Node 6 Empty Empty) Empty
d = Node 6 Empty (Node 1 Empty Empty)

-- should make sure value can't be changed
ivariant :: Ord a => a -> SkewHeap a 
ivariant a = undefined

singleton :: Ord a => a -> SkewHeap a
singleton x = Node x Empty Empty

-- rank :: SkewHeap a -> a
-- rank Empty          = 0
-- rank (Node _ _ r )  = r + 1

merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge h1 Empty = h1
merge Empty h2 = h2
merge h1@(Node x1 l1 r1) h2@(Node x2 l2 r2)
    | x1 <= x2  = Node x1 (merge r1 h2) l1
    | otherwise = Node x2 (merge r2 h1) l2


insert :: Ord a => a -> SkewHeap a -> SkewHeap a 
insert x sh = merge (singleton x) sh 

extracte_min :: Ord a => SkewHeap a -> SkewHeap a 
extracte_min h@(Node x l r ) = undefined

delete :: Ord a => SkewHeap a -> SkewHeap a 
delete h = undefined

