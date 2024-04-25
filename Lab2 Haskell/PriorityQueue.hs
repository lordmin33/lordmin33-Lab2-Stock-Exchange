module PriorityQueue where

-- Add a  heap sort so that the skewheap is implemented correctly
data SkewHeap a = Empty
                | Node a (SkewHeap a) (SkewHeap a) deriving (Show, Eq)

-- used when inserting a new element to the heap
singleton :: Ord a => a -> SkewHeap a -- O(1)
singleton x = Node x Empty Empty

-- merges two skew heaps into one
merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a -- O(n)
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
findLargest heap = case extractMin heap of
    Nothing -> Nothing -- If the heap is empty, return Nothing
    Just (minElem, restHeap) -> case restHeap of
        Empty -> Just minElem -- If the rest of the heap is empty, minElem is the largest value
        _     -> findLargest restHeap -- Otherwise, continue searching in the rest of the heap


-------------- have not gotten this to work propperly yet -----------------
data MaxHeap a = EmptyMax | MaxNode a (MaxHeap a) (MaxHeap a) deriving (Show)

singletonM :: Ord a => a -> MaxHeap a -- O(1)
singletonM x = MaxNode x EmptyMax EmptyMax

mergeMax :: Ord a => MaxHeap a -> MaxHeap a -> MaxHeap a -- O(log(n))
mergeMax h1 EmptyMax = h1
mergeMax EmptyMax h2 = h2
mergeMax h1@(MaxNode x1 l1 r1) h2@(MaxNode x2 l2 r2)
    | x1 >= x2  = MaxNode x1 (mergeMax r1 h2) l1
    | otherwise = MaxNode x2 (mergeMax r2 h1) l2

insertM :: Ord a => a -> MaxHeap a -> MaxHeap a -- O(log(n))
insertM x sh = mergeMax (singletonM x) sh

deleteM :: Ord a => a -> MaxHeap a -> MaxHeap a -- O(log(n))
deleteM _ EmptyMax = EmptyMax
deleteM y (MaxNode x l r)
    | y == x    = mergeMax l r
    | otherwise = MaxNode x (deleteM y l) (deleteM y r)

toSortedListM :: Ord a => MaxHeap a -> [a] -- O(n*log(n))
toSortedListM EmptyMax          = []
toSortedListM (MaxNode x l r)   = x : toSortedListM (mergeMax l r)




