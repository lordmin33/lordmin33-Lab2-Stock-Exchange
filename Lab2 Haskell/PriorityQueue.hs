module Lab2 where

data SkewHeap a = Empty
                | Node a (SkewHeap a) (SkewHeap a) deriving (Show)

-- should make sure value can't be changed
ivariant :: Ord a => a -> SkewHeap a
ivariant a = 

singleton :: Ord a => a -> SkewHeap a
singleton x = Node x Empty Empty

rank :: SkewHeap a -> a



merge :: Ord a =>  SkewHeap a -> SkewHeap a -> SkewHeap a
merge sh1 Empty = sh1 
merge Empty sh2 = sh2 
merge sh1@(Node a1 b1 c1) sh2@(Node a2 b2 c2) 
            | a1 < a2 = merge sh1 sh2
            | a2 < a1 = merge sh2 sh1
            | otherwise = sh1 

insert :: Ord a => a -> SkewHeap a -> SkewHeap a 
insert x sh = merge (singleton x) sh 

extracte_min :: Ord a => SkewHeap a -> SkewHeap a 
extracte_min sh = undefined

delete :: Ord a => SkewHeap a -> SkewHeap a 
delete sh = undefined


data LHeap a = Leaf | Node' a (LHeap a) (LHeap a) deriving (Show)

rank' :: LHeap a -> Integer
rank' Leaf = 0
rank' (Node' _ _ r) = rank r + 1

merge' :: Ord a => LHeap a -> LHeap a -> LHeap a
merge' Leaf h = h
merge' h Leaf = h
merge' h@(Node' a l r) h'@(Node' a' _ _)
  | a > a'           = merge' h' h
  | rank r' > rank l = Node' a r' l
  | otherwise        = Node' a l r'
  where r' = merge' r h'

