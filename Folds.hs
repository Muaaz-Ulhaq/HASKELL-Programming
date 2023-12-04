--Q1.--
sumSequence :: (Num b, Enum b) => b -> b
sumSequence n = foldr(\n acc-> (acc+(n*n))) 0 [1..n]
--Q2.--
sumSequence' :: (Num a, Enum a) => a -> a
sumSequence' n=sum (map (^2) [1..n])
--Q3.--
minPos :: (Foldable t, Ord a, Num a) => t a -> a
minPos xs = foldl1(\acc x->if(x<acc && x>0)then x else acc) xs
--Q4.--
pairWithSquare :: (Foldable t, Num b) => t b -> [(b, b)]
pairWithSquare xs = foldr(\x acc-> (x,(x^2)):acc) [] xs
--Q5.--
remdupicates :: (Foldable t, Eq a) => t a -> [a]
remdupicates xs = foldr(\x acc-> if(x `elem` acc)then acc else x:acc) [] xs
--Q6.--
remdupicates' :: (Foldable t, Eq a) => t a -> [a]
remdupicates' xs = foldl(\acc x-> if(x `elem` acc)then acc else acc++[x] ) [] xs
--Q7.--
initials :: Foldable t => t a -> [[a]]
initials xs = foldr(\x acc->[]:map (x:) acc) [[]] xs
--Q8.--
shiftIt :: [a] -> [a]
shiftIt (x:xs) = xs++[x]
--Q9.--
rotateIt :: [a] -> [[a]]
rotateIt xs = foldl(\acc x->x:acc) [] (performRotation (length xs) xs)
performRotation len (x:xs)
    |len < 1=[]
    |otherwise = (x:xs):performRotation (len-1) (shiftIt(x:xs))


