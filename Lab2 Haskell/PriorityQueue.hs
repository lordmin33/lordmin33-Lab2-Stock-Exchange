module Lab2 where

data SkewHeap a = Empty
                | Node a (SkewHeap a) (SkewHeap a) deriving (Show)

-- should make sure value can't be changed
ivariant :: Ord a => a -> SkewHeap a
ivariant a = undefined

singleton :: Ord a => a -> SkewHeap a
singleton x = node x Empty Empty

merge :: Ord a =>  SkewHeap a -> SkewHeap a -> SkewHeap a
merge sh1 Empty = sh1 
merge Empty sh2 = sh2 
merge sh1 sh2 

insert :: Ord a => a -> SkewHeap a -> SkewHeap a 
insert a sh = merge singleton x sh 

extracte_min :: Ord a => SkewHeap a -> SkewHeap a 
extracte_min sh = undefined

delete :: Ord a => SkewHeap a -> SkewHeap a 
delete sh = undefined


