module Merge where

    merge :: Ord a => [a] -> [a] -> [a]
    merge left [] = left
    merge [] right = right
    merge (x:xs) (y:ys) 
        | x >= y = y : merge ys (x:xs)
        | otherwise = x : merge (y:ys) xs 
    
    natNums = 0 : map (+1) natNums

    -- habe hier mal eine random primes definition genommen
    primes = sieve [2..]

    sieve (p : xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]

    getPowerList nbr = [1..floor (logBase 2 nbr)]

    same :: Integer -> [Integer]
    same x = x : same x


    --diese aufgabe hat mich viel zu lang gebraucht

    primepowers :: Int -> [Integer]
    primepowers n = take n (foldl (\a b -> merge b a) [] ( take n (getMultipliedPrimeLists n)))

    getMultipliedPrimeLists :: Integral b => b -> [[Integer]]
    getMultipliedPrimeLists n =  map (\list->map (\tuple->(fst tuple) ^ (snd tuple)) list) (( map (\x->(zip (same x) [1..n])) primes))
    

