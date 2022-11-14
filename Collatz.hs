module Collatz where

    collatzCalc :: Integer -> Integer
    collatzCalc nbr
        | even nbr = nbr `div` 2
        | otherwise = (3*nbr)+1
    
    collatz :: Integer -> [Integer]
    collatz nbr = iterate collatzCalc nbr

    natNums = 0 : map (+1) natNums


    num startValue = smallestCollatzCalc (zip natNums (collatz startValue))

    smallestCollatzCalc current
        | snd (head current) == 1 = fst (head current)
        | otherwise = smallestCollatzCalc (drop 1 current)

    maxNum bottom top = foldl (\x y->if snd x > snd y then x else y) (bottom,0) (zip [bottom..top] (map num [bottom..top])) 