module PriorityQueue where

-- Add a  heap sort so that the skewheap is implemented correctly
data SkewHeap a = Empty
                | Node a (SkewHeap a) (SkewHeap a) deriving (Show, Eq)

-- used when inserting a new element to the heap
singleton :: Ord a => a -> SkewHeap a -- O(1)
singleton x = Node x Empty Empty

-- merges two skew heaps into one
merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a -- O(log(n))
merge Empty Empty = Empty
merge h1 Empty  = h1
merge Empty h2  = h2
merge h1@(Node x1 l1 r1) h2@(Node x2 l2 r2)
    | x1 <= x2  = Node x1 (merge r1 h2) l1
    | otherwise = Node x2 (merge r2 h1) l2

-- inserts values into the heap using the singleton method
insert :: Ord a => a -> SkewHeap a -> SkewHeap a -- O(log(n))
insert x sh = merge (singleton x) sh 

-- Extract the minimum value of the heap and merge the rest  
extractMin :: Ord a => SkewHeap a -> Maybe (a, SkewHeap a) -- O(log n)
extractMin Empty        = Nothing
extractMin (Node x l r) = Just (x, merge l r)

-- deletes the first instance of a specific value in the heap
delete :: Ord a => a -> SkewHeap a -> SkewHeap a --O(n)
delete _ Empty = Empty -- If the heap is empty, return Nothing
delete y (Node x l r)
    | y == x    = merge l r -- If the root value equals the value to delete, return Just x and merge the left and right subtrees
    | otherwise = Node x (delete y l) (delete y r)  -- If the value to delete is greater than the root value, recursively delete from the right subtree

toSortedList :: Ord a => SkewHeap a -> [a] -- O(n)
toSortedList Empty          = []
toSortedList (Node x l r)   = x : toSortedList (merge l r) 

findLargest :: Ord a => SkewHeap a -> Maybe a
findLargest Empty = Nothing
findLargest heap = Just $ goRight heap
  where
    goRight (Node x _ Empty) = x
    goRight (Node _ _ right) = goRight right



reverseHeap :: SkewHeap a -> SkewHeap a
reverseHeap Empty = Empty
reverseHeap (Node x left right) = Node x (reverseHeap right) (reverseHeap left)

{-data MaxSkewHeap a = EmptyMax | MaxNode a (MaxSkewHeap a) (MaxSkewHeap a) deriving (Show)

-- Merge two max skew heaps
mergeMax :: Ord a => MaxSkewHeap a -> MaxSkewHeap a -> MaxSkewHeap a
mergeMax h EmptyMax = h
mergeMax EmptyMax h = h
mergeMax h1@(MaxNode x1 l1 r1) h2@(MaxNode x2 l2 r2)
    | x1 >= x2  = MaxNode x1 (mergeMax r1 h2) l1
    | otherwise = MaxNode x2 (mergeMax r2 h1) l2

-- Convert a skew heap into a max skew heap
toMaxSkewHeap :: Ord a => SkewHeap a -> MaxSkewHeap a
toMaxSkewHeap Empty = EmptyMax
toMaxSkewHeap (Node x l r) = MaxNode x (toMaxSkewHeap r) (toMaxSkewHeap l)-}