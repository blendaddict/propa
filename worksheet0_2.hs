qsort :: (Ord t) => [t] -> [t]
qsort [] = []
qsort (p:ps) = (qsort (filter ( <= p) ps))++ p:(qsort (filter ( > p) ps))
