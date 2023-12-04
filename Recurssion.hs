--Q1
insertInPlace :: Ord t => t -> [t] -> [t]
insertInPlace y []=y:[]
insertInPlace y (x:xs)
    | y<x = quicksort (y:x:xs)
    | otherwise = x:(insertInPlace y xs)
--Q2 (a)
kmtomiles :: (Eq a, Num a, Num b, Enum a) => a -> [b]
kmtomiles n = map fibonacci [1..n]
--Q2 (b)
findMiles :: Int -> Int
findMiles n = findVal n xs
    where xs=kmtomiles n
--Q3 (a)	
secondToMax :: Ord a => [a] -> a
secondToMax xs = last(init(quicksort xs))
--Q3 (b)
splitList :: Ord a => [a] -> ([a], [a])
splitList [] = ([],[])
splitList (x:[]) = ([],[x])
splitList (x:xs) =  
         if x> head xs then (p,x:q)
         else (x:p,q)
         where (p,q) = splitList xs
--Q4
n :: (Num a, Num b, Eq a, Eq b) => (a, b) -> b
n(a, b)
    |a==0 = b+1
    |b==0 = n(a-1,1)
    |otherwise = n(a-1,n(a,b-1))

-- Extra Functions
quicksort [] = []  
quicksort (x:xs) =   
    let smallerList = quicksort [a | a <- xs, a <= x]  
        biggerList = quicksort [a | a <- xs, a > x]  
    in  smallerList ++ [x] ++ biggerList  

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

returnVal n []= False
returnVal n (x:xs)
    | x==n = True
    | otherwise = n `returnVal` xs

findVal n xs
    | returnVal n xs == True = n
    | returnVal n xs == False = xs !! i
    where i=n-2

length' xs = sum[1|x<-xs] 

	