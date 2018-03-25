unsorted_list = [6, 7, 4, 45, 7, 76, 3, 67, 7, 63, 19]
sorted_list = [2, 6, 10, 14, 55, 65, 78, 99, 102]

-- purpose: search a data structure for a chosen element
-- input: haystack -> data structure to search
--       needle -> element to search for
--       current_item -> function that returns the current item at the front
--       done -> function that returns true if search stops with failure
--       found -> function that returns true if current item is the element
--       next -> function that returns the part of the data to be searched next
-- output: element if found or nil if not found
--search haystack needle current_item done found next
-- | done needle haystack = []
-- | needle found (current_item haystack) = current_item haystack
-- | otherwise = search (next needle haystack) needle current_item done found next
--search (x:xs) y found = found x y 
--search (x:xs) y current_item done found next = current_item (x:xs)
search lst elm current_item done found next
 | done elm lst = []
 | found elm (current_item lst) = [current_item lst] 
 | otherwise = search (next elm lst) elm current_item done found next 

-- done functions
second_empty elm lst
 | lst == [] = True
 | otherwise = False
--second y lst = null lst --(same as above)
stop_greater y lst = second_empty y lst || y < (head lst)

-- next functions
tail_second y (x:xs) = xs 
 
-- found (==)

--search unsorted_list 6 head second_empty (==) tail_second
