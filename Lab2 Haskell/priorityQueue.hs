data SkewHeap a = Empty
                | Node a (SkewHeap a) (SkewHeap a)





delete :: Ord a -> SkewHeap a -> SkewHeap a 
