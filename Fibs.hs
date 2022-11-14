module Fibs where
    
    fibCalc :: (Eq t, Num t, Num a) => t -> a
    fibCalc 0 = 0
    fibCalc 1 = 1
    fibCalc int = fibCalc (int-1) + fibCalc (int-2)

    natNums = 0 : map (+1) natNums

    fibs = map fibCalc natNums

    --keine Ahnung wie ich hier zipWith hätte verwenden sollen

    