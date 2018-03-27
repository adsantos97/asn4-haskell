data Tree = Nil | Node (Integer, Tree, Tree)
  deriving Show

t = Node(5, Node(3, Nil, Node(4, Nil, Nil)), Nil)
bst = Node(10, 
        Node(8, 
          Node(4, Nil, Node(6, Nil, Nil)), 
          Node(9, Nil, Nil)), 
        Node(15, Nil, Node(18, Nil, Nil)))

--ints in tree
iit Nil = []
iit (Node(v,l,r)) = (iit l) ++ [v] ++ (iit r)

--found needle Nil = False
found needle (Node(v, l, r)) = if needle == v then True
                               else False

done needle Nil = True
done needle (Node(v, l, r)) = False

thead (Node(v, l, r)) = v

search needle Nil = []
search needle (Node(v, l, r))
 | needle == v = [v]
 | needle < v = search needle l
 | otherwise = search needle r

tnext needle (Node(v, l, r))
 | needle < v = l
 | otherwise = r

search2 haystack needle current_item done found next
 | done needle haystack = []
 | found needle (current_item haystack) = [(current_item haystack)]
 | otherwise = search2 (next needle haystack) needle current_item done found next
