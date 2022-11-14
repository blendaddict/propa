module Sort where 
    
    insert [] element = element:[]
    insert (s:xs) element
        | s >= element = element:s:xs
        | otherwise = s : insert xs element

    insertSortHelper [] newList = newList
    insertSortHelper (x1:xs1) sorted = insertSortHelper xs1 (insert sorted x1)  
    
    insertSort [] = []
    insertSort unsorted = insertSortHelper unsorted []

    merge (x:xs) [] = (x:xs)
    merge [] (y:ys) = (y:ys)
    merge (x:xs) (y:ys) 
        | x < y = x : merge xs (y:ys)
        | otherwise = y : merge (x:xs) ys 
    
    mergeSort list
        | list == [] = []
        | length list == 1 = list
        | otherwise = merge (mergeSort(take ((length list) `div` 2) list)) (mergeSort(drop ((length list) `div` 2) list)) 