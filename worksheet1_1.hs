max3 e1 e2 e3
    | (max e1 e2 == e1 && max e1 e3 == e1) = e1
    | (max e2 e3 == e2 && max e2 e1 == e2) = e2
    | otherwise = e3

-- max  [] = []
-- max (p:ps) = (qsort (filter ( <= p) ps))++ p:(qsort (filter ( > p) ps))
