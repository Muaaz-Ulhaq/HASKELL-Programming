bubbleUp []=[]
bubbleUp [x] = [x]
bubbleUp (x:xs)
    | x > head xs = head xs : bubbleUp (x : tail xs)
    | otherwise = x : bubbleUp xs
 
bubbleSort []=[]
bubbleSort xs
    | xs == list =xs
    | otherwise = bubbleSort list
    where list= bubbleUp xs