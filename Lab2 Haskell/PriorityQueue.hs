module Lab2 where

data SkewHeap a = Empty
                | Node a (SkewHeap a) (SkewHeap a) deriving (Show)

ivariant :: Ord a => a -> SkewHeap a
ivariant a = a

singleton :: Ord a => a -> SkewHeap a
singleton x = node x Empty Empty

merge :: Ord a =>  SkewHeap a -> SkewHeap a -> SkewHeap a
merge sh1 sh2 = undefined

insert :: Ord a => a -> SkewHeap a -> SkewHeap a 
insert a sh = undefined

extracte_min :: Ord a => SkewHeap a -> SkewHeap a 
extracte_min sh = undefined


delete :: Ord a -> SkewHeap a -> SkewHeap a 
delete sh = undefined
