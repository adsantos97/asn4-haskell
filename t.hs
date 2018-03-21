module Examples where

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

