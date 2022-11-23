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

    -- primepowers :: Int -> [Integer]
    -- primepowers n = foldl merge [] ( (getMultipliedPrimeLists n))

    -- recursiveThingy listOfLists = 

    -- getMultipliedPrimeLists :: Integral b => b -> [[Integer]]
    -- getMultipliedPrimeLists n =  map (\list->map (\tuple->(fst tuple) ^ (snd tuple)) list) (( map (\x->(zip (same x) [1..n])) primes))
    
    -- getSecondMultipliedPrimeLists n = (map (\nbr->drop (nbr-1) primes) [1..n])

    -- primepowers n = (getSecondMultipliedPrimeLists n)

    primepowers :: Int -> [Int]
    primepowers n = foldl merge [] (map primepowerlist [1..n])
--primepowers n = foldl merge [] (map (take n . primepowerlist) primes)

    primepowerlist :: Int -> [Int]
    primepowerlist n = map (^n) primes