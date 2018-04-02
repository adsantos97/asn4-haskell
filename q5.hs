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

-- non-tail recursive of matches function
matches x [] = 0
matches x lst
 | x == (head lst) = 1 + matches x (tail lst)
 | otherwise = matches x (tail lst)

-- tail recursive of matches function
matches_helper [] x rsf = rsf
matches_helper lst x rsf
 | x == (head lst) = matches_helper (tail lst) x (1 + rsf)
 | otherwise = matches_helper (tail lst) x rsf 

matches_tail x lst = matches_helper lst x 0

-- fold of matches function
add_match a b = if a == b then b + 1 else b 

matches_fold x lst = fold lst 0  tail
