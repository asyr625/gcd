--[1..10^12]
--sum ::
--sum  = map filter xs

divided :: Int -> Int -> Bool
divided a n =  n `mod` a == 0

addDivide :: Int -> Int -> Int
addDivide a n = if divided a n 
				 then a + (n `div` a)
				 else 0


sumN :: [Int] -> Int -> Int
sumN (x:xs) n = addDivide x n + sumN xs n
sumN _ _ = 0
{- 
sumN (x:xs) n ss = sumN xs n (ss + addDivide x n)
sumN _ _  ss = ss
-}

mysum 1 = 1
mysum 2 = 3
mysum 4 = 7
mysum n = if divided 2 n 
		then sumN [1..(n `div` 2 - 1)] n 
		else sumN [1..((n+1) `div` 2)] n 
		
		
--sumSum (x:xs) n = sumSum xs (n + mysum x)
--sumSum _ n = n

sumSum (x:xs) =mysum x +  sumSum xs 
sumSum _ = 0

			   