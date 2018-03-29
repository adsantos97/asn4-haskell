fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fib n
 | n == 0 = [0]
 | otherwise = fib (n-1) ++ [fibonacci n]

data Seq a = Nil | Cons (a, (() -> Seq a))

instance (Show a) => Show (Seq a) where
  show Nil = "Nil"
  show (Cons (a, f)) = "Cons(" ++ show a ++ ", fn())"

hd Nil = error "HD_NIL"
hd (Cons (x, f)) = x

tl Nil = error "TL_NIL"
tl (Cons (x, f)) = f()

from k = Cons(k, \() -> from (k+1))


