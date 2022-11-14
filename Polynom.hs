module Polynom where   
    
    type Polynom = [Double]

   
    cmult :: Polynom -> Double -> Polynom
    cmult pol const = map (* const) pol 

    eval :: Polynom -> Double -> Double
    eval poly x = foldr (\c acc -> c + x * acc) 0.0 poly

    natNums = 0 : map (+1) natNums

    deriv :: Polynom -> Polynom
    deriv poly = drop 1 (map (\tuple->fst(tuple) * snd (tuple)) (zip poly (take (length poly) natNums)))