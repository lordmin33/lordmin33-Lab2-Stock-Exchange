data SkewHeap a = Empty
                | Node a (SkewHeap a) (SkewHeap a)


merge :: Ord a ->  SkewHeap a -> SkewHeap a -> SkewHeap a
merge = undefined

add_value :: Ord a -> a -> SkewHeap a -> SkewHeap a 
add_value = undefined

delete_min :: Ord a -> SkewHeap a -> SkewHeap a 
delete_min = undefined


delete :: Ord a -> SkewHeap a -> SkewHeap a 
delete = undefined