data Seq a = Nil | Cons (a, (() -> Seq a))

instance (Show a) => Show (Seq a) where
  show Nil = "Nil"
  show (Cons (a, f)) = "Cons(" ++ show a ++ ", fn())"

-- head of a sequence
hd Nil = error "HD_NIL"
hd (Cons (x, f)) = x

-- tail of a sequence
tl Nil = error "TL_NIL"
tl (Cons (x, f)) = f()

-- generates the next sequence
from k = Cons(k, \() -> from (k+1))

-- purpose: generate the n-th term of fibonacci numbers
-- input: n -> which term from the sequence
-- output: n-th term of the sequence
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- fibonacci numbers from n-th to 10th term
fib 11 = []
fib n = fibonacci n : fib (n+1)

-- purpose: fibonacci sequence up to 10-th term
-- input: seq to start from
-- output: fibonacci numbers starting from seq to 10 (including 10-th term)
fib_seq (Cons(11,_)) = []
fib_seq seq = fibonacci (hd seq) : fib_seq (tl seq)

-- lazy Fibonacci number generator
fib_stream seq = Cons (fibonacci (hd seq), \() -> fib_stream (tl seq))

-- test number generator
ps = fib_stream (from 0)
ps_hd = hd ps -- test head of sequence
ps_tl = tl ps -- next sequence

-- test number generator (from 0) to (from 10) -> using "tl it"
-- 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55
  
