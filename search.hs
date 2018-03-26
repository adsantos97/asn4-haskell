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
search haystack needle current_item done found next
 | done needle haystack = []
 | found needle (current_item haystack) = [current_item haystack] 
 | otherwise = search (next needle haystack) needle current_item done found next 

-- done functions
second_empty first second = null second 
stop_greater x lst = second_empty x lst || x < (head lst)

-- next functions
tail_second y (x:xs) = xs 
 
-- found functions
-- all use (==)

-- test calls
{-
search unsorted_list 6 head second_empty (==) tail_second -- first in list
search unsorted_list 19 head second_empty (==) tail_second -- last in list
search unsorted_list 3 head second_empty (==) tail_second -- middle in list
search unsorted_list 9 head second_empty (==) tail_second -- not in list

search sorted_list 2 head stop_greater (==) tail_second -- first in list
search sorted_list 102 head stop_greater (==) tail_second -- last in list
search sorted_list 55 head stop_greater (==) tail_second -- middle in list
search sorted_list 40 head stop_greater (==) tail_second -- not in list
search sorted_list 1 head stop_greater (==) tail_second -- not in list before first
search sorted_list 777 head stop_greater (==) tail_second -- not in list beyond last
-}
