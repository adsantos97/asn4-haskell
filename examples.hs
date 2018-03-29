module Examples where

-- (define add (lambda (x y) ( + x y))

add = \x y -> x + y -- curried version; Integer -> (Integer -> Integer)
addp = \(x,y) -> x + y

-- Homework:
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

member n [] = False
member n (x:xs)
  | n == x = True
  | otherwise = member n xs

f0 x = x + 1 -- type: int -> int
f1 = \x -> x + 1
f2 = \() -> 42 -- constant

-- (define add (lambda (x y) ( + x y)))

add = \x y -> x + y
addp = \(x,y) -> x + y

-- (define (add x y) (+ x y))
addx x y = x + y

fact n = if n == 0 then 1 else n * fact (n-1)
--         n == 0 ? 1 : n * fact(n-1)

-- foo  n = if n == 0 then 1 else n * foo n-1
-- foo  n = if n == 0 then 1 else n * (foo n) - 1

fact2 0 = 1
fact2 n = n * fact2 (n-1)

fact3 n
  | n == 0 = 1
  | otherwise = n * fact3 (n-1)


len0 lst = if lst == [] then 0 else 1 + len (tail lst)

len [] = 0
len lst  = 1 + len (tail lst)

len2 []     = 0
len2 (x:xs) = 1 + len xs

len3 []     = 0
len3 (_:xs) = 1 + len xs

f (x:_) = x

append []     ys = ys
append (x:xs) ys = x : append xs ys


mymap f [] = []
mymap f (x:xs) = f x  :  mymap f xs


fold0 []  finish next gen = finish
fold0 lst finish next gen = next (head lst) (fold0 (gen lst) finish next gen)

fold []  finish next gen = finish
fold (x:xs) finish next gen = next x (fold (gen (x:xs)) finish next gen)

-- (:)  is the cons function
-- (+) is the add function 

inc_second a b = b + 1

sumlist l = fold l 0 (+) tail
productlist l = fold l 1 (*) tail
lenf l = fold l 0 inc_second tail

app a b = fold a b (:) tail

exclude x     [] = [x]
exclude x (y:ys) = if x == y then (y:ys) else (x:y:ys)

-- exclude z     [] = [z]
-- exclude z (z:ys) = (y:ys)
-- exclude z (y:ys) = (z:y:ys)

remdup lst = fold lst [] exclude tail


data Mine = AA | BB
    deriving Show
-- data char = A|B|C|D|E|F|G|H....

f2 AA = 0
f2 BB = 1

data Tree = NIL | Node (Integer, Tree, Tree)
    deriving Show

t = Node(5,Node(3,NIL,Node(4,NIL,NIL)),NIL)

-- ints_in_tree
iit NIL = []
iit (Node (v,l,r)) = (iit l)  ++  [v] ++  (iit r)

-- iit (Node (v,l,r)) = (iit l)  ++  (v:iit r)


data Tree_of_x x = Nil | Nodex (x, Tree_of_x x, Tree_of_x x)
    deriving Show

tx = Nodex(5,Nil,Nil)
ty = Nodex(True,Nil,Nil)
