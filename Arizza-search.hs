unsorted_list = [6, 7, 4, 45, 7, 76, 3, 67, 7, 63, 19]
sorted_list = [2, 6, 10, 14, 55, 65, 78, 99, 102]

data Tree = Nil | Node(Integer, Tree, Tree)
  deriving Show

t = Node(5, Node(3, Nil, Node(4, Nil, Nil)), Nil)
bst = Node(10, Node(8, Node(4, Nil, Node(6, Nil, Nil)), Node(9, Nil, Nil)), Node(15, Nil, Node(18, Nil, Nil)))

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

-- current_item functions
-- head used for sorted and unsorted lists
thead (Node(v, l, r)) = v

-- done functions
second_empty first second = null second 
stop_greater x lst = second_empty x lst || x < (head lst)

-- purpose: done function for tree
-- input: a -> needle
--        tree
-- output: true if tree is Nil, otherwise false
tempty a Nil = True
tempty a (Node(v, l, r)) = False
 
-- found functions
-- all use (==)

-- next functions
tail_second x lst = tail lst

-- purpose: next function for tree
-- input: x -> needle
--        tree
-- output: left subtree if needle is less than the current node
--         otherwise, right subtree
tnext x (Node(v, l, r))
 | x < v = l
 | otherwise = r 

-- test calls -> easily called by typing the letters
a =  search unsorted_list 6 head second_empty (==) tail_second -- first in list
b =  search unsorted_list 19 head second_empty (==) tail_second -- last in list
c =  search unsorted_list 3 head second_empty (==) tail_second -- middle in list
d =  search unsorted_list 9 head second_empty (==) tail_second -- not in list

e =  search sorted_list 2 head stop_greater (==) tail_second -- first in list
f =  search sorted_list 102 head stop_greater (==) tail_second -- last in list
g =  search sorted_list 55 head stop_greater (==) tail_second -- middle in list
h =  search sorted_list 40 head stop_greater (==) tail_second -- not in list
i =  search sorted_list 1 head stop_greater (==) tail_second -- not in list before first
j =  search sorted_list 777 head stop_greater (==) tail_second -- not in list beyond last

k =  search bst 10 thead tempty (==) tnext -- first in tree (root)
l =  search bst 4 thead tempty (==) tnext -- in left
m =  search bst 15 thead tempty (==) tnext -- in right
n =  search bst 11 thead tempty (==) tnext -- not in tree
