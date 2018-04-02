unsorted_list = [6, 2, 7, 1]

-- non-tail recursive of smallest function
--smallest lst = minimum lst

-- tail recursive of smallest function
smallest [x] = x
smallest (x:xs)
 | x < (smallest xs) = x
 | otherwise = smallest xs
