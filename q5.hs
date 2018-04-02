unsorted_list = [6, 2, 7, 1]

-- non-tail recursive of smallest function
smallest [x] = x
smallest lst = if (head lst) < (head (tail lst))
                 then smallest lst
               else smallest ((head (tail lst)):(tail lst))

-- tail recursive of smallest function
small [x] = x
small (x:y:xs) = if x < y then small (x:xs)
                 else small (y:xs)
