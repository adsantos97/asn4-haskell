ul = [6, 2, 7, 1]

-- fold function
fold [] finish next gen = finish
fold (x:xs) finish next gen = next x (fold (gen (x:xs)) finish next gen)

-- non-tail recursive of smallest function
smallest [x] = x
smallest lst
 | (head lst) < (smallest (tail lst)) = head lst
 | otherwise = smallest (tail lst)

-- tail recursive of smallest function
smallest_helper [] rsf = rsf
smallest_helper lst rsf
 | (head lst) < rsf = smallest_helper (tail lst) (head lst)
 | otherwise = smallest_helper (tail lst) rsf

smallest_tail lst = smallest_helper lst (head lst)

-- fold of smallest function
compare_smallest a b = if a < b then a else b 

smallest_fold lst = fold lst (head lst) compare_smallest tail


