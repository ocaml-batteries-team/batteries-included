print_string test
  where test = "test_where succeeded"

let enum = ( 1 -- 999 ) // odd 
 where odd x = 
   x mod 2 = 1
 and even x = 
   x mod 2 = 0

let fibo n = fst (fibo_aux n)
  where rec fibo_aux = function
    | 0 -> (1, 1)
    | 1 -> (1, 2)
    | n -> (a, a + b) where let (a, b) = fibo_aux ( n - 1 )

