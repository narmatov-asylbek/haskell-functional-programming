module Generator where
    fib 0 = 0
    fib 1 = 1
    fib n = 
        let
            iter a b 0 = a
            iter a b n = iter b (b + a) (n - 1)
        in
            iter 0 1 n
    
    infiniteFib n = fib n : [fib (n + 1)]
    -- fibStream = zipWith fib (\x -> x + 1)