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
search [] y current_list = "empty"
search (x:xs) y current_list = current_list (x:xs)





-- done functions
second_empty _ second = null second

-- next functions
tail_second _ b = tail b 
 
-- found (==)

--search unorted_list '6 head second_empty (==) tail_second
