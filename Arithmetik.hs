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

    pow3 = powAcc 1

    -- powAcc b e acc
    --     | e == 0 = acc
    --     | odd e = powAcc  
    --     |

    powAcc:: a -> a -> a -> a
    powAcc acc b 0 = acc
    powAcc acc b e
        | odd e = powAcc (acc * b) (b * b) (div e 2)
        | otherwise = powAcc acc (b * b) (div e 2) 

    pow3 = powAcc' 1

    powAcc' acc b 0 = acc
    powAcc' acc b e
        | e `mod` 2 == 0 = powAcc' acc (b * b) (e `div` 2)
        | otherwise      = powAcc' (b * acc) (b * b) (e `div` 2)

    -- root e r 
    --     | (r >= 0 && e >= 0) = half e r 0 r
    --     | otherwise = error "invalid input"


    half e r bot top
        | (top-bot)==1 = bot
        | pow2 (bot + ((top-bot) `div` 2)) e <= r =  half e r (bot + ((top-bot) `div` 2)) top 
        | otherwise = half e r bot (top - (top-bot) `div` 2)

    

    