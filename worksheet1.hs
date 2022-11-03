module Arithmetik where
    
    pow1 b 0 = 1
    pow1 b e = b * (pow1 b (e-1))

    pow1Acc b e acc = if e == 0 then acc else pow1Acc b (e-1) acc*acc 
    pow1New b e = pow1Acc b e b

    pow2 b 0 = 1
    pow2 b 1 = b
    pow2 b 2 = b*b
    pow2 b e 
        | odd e = b * pow2 (pow2 b 2) (div e 2)
        | otherwise = pow2 (pow2 b 2) (div e 2)
    --log(n) aufrufe

    pow3 b e = pow2Acc b e 1 
    
    pow2Acc b e acc
        | e == 0 = acc
        | e == 1 = acc * b
        | e == 2 = acc * b * b
        | odd e = pow2Acc b (div e 2) b*b*b
        | otherwise = pow2Acc b (div e 2) b*b