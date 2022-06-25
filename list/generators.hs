module Generator where
    import Prelude hiding(repeat, replicate, cycle, iterate)
    f a b = a ++ f b (zipWith (+) a  b)
    
    fibStream :: [Integer]
    fibStream = f [0] [1]
    -- fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)


    repeat :: a -> [a]
    repeat x = xs where xs = x : xs


    replicate :: Int -> a -> [a]
    replicate n x = take n (repeat x)

    cycle :: [a] -> [a]
    cycle [] = error "cycle: Empty list"
    cycle xs = ys where ys = xs ++ ys


    iterate :: (a -> a) -> a -> [a]
    iterate f x = x : iterate f (f x)