unsorted_list = [6, 2, 7, 1]

-- non-tail recursive of smallest function
smallest_helper [] rsf = rsf
smallest_helper lst rsf
 | (head lst) < rsf = smallest_helper (tail lst) (head lst)
 | otherwise = smallest_helper (tail lst) rsf

smallest lst = smallest_helper lst (head lst)

-- tail recursive of smallest function
smallest [x] = x
smallest lst
 | (head lst) < (smallest (tail lst)) = head lst
 | otherwise = smallest (tail lst)
