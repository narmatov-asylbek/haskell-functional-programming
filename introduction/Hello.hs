{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Test where
    import Data.Char
    letsayhello = putStrLn "Hello from module test!"
    sumSquares x y = x ^ 2 + y ^ 2
    lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)
    fortyTwo = 42
    sign x = if x > 0 then 1 else (if x == 0 then 0 else - 1)
    max5' x = max 5 x
    max5 = max 5

    infixl 6 *+*
    a *+* b = a ^ 2 + b ^ 2
    x |-| y = abs ((-) x y)
    y = 5 :: Double
    z = 6 :: Integer

    test = isDigit

    twoDigits2Int x y =
        if isDigit x && isDigit y
            then digitToInt x * 10 + digitToInt y
                else 100

    dist p1 p2 = sqrt $ (fst p2 - fst p1) ^ 2 + (snd p2 - snd p1) ^ 2
    factorial n = if n == 0 then 1 else n * factorial (n - 1)

    factorial2 0 = 1
    factorial2 n = n * factorial (n - 1)

    doubleFact 0 = 1
    doubleFact n = if n < 0 then error "Should be non negative" else n * doubleFact (n - 2)


    factorial3 n | n < 0 = error "must be > 0"
                 | n > 0 = n * factorial3 (n - 1)
                 | otherwise = 1


    fibonacci n  | n == 0 = 0
                 | n == 1 = 1
                 | n == (- 1) = 1
                 | n < 0 = - (fibonacci (n + 1) - fibonacci (n + 2))
                 | otherwise = fibonacci (n - 1) + fibonacci (n - 2)


    factorial5 n | n >= 0 = helper 1 n
                 | otherwise = error "must be greater than 0"

    helper acc 0 = acc
    helper acc n = helper (acc * n) (n - 1)

    fibonacci2 = helper2 0 1 0

    helper2 a b counter n | n == counter && n >= (- 1) = a
                          | n == counter && n < ( - 1) = - a
                          | n < 0 = helper2 b (a + b) (counter - 1) n
                          | otherwise = helper2 b (a + b) (counter + 1) n

    isTrue = let x = True in x
    max5and10 = let {x = 10; y = 5} in max x y



    factorial6 n
        | n >= 0 =
            let
                helper acc 0 = acc
                helper acc n = helper (acc * n) (n - 1)
            in
                helper 1 n
        | otherwise = error "args must be >= 0"

    seqA n
        | n <= 0 = 1
        | n == 1 = 2
        | n == 2 = 3
        | otherwise = seqA (n - 1) + seqA (n - 2) - 2 * seqA (n - 3)

    seqB n
        | n <= 0 = 1
        | n == 1 = 2
        | n == 2 = 3
        | otherwise =
            let
                calculate a b c = 
                    c + b - 2 * a
                helper a b c counter
                    | n == counter = calculate a b c
                    | otherwise = helper b c (calculate a b c) (counter + 1)
            in
                helper 1 2 3 3

    seqC n = 
        let
            helper 0 a3 a2 a1 = a1
            helper n a3 a2 a1 = helper (n - 1) (a3 + a2 - 2 * a1) a3 a2
        in
            helper n 3 2 1


    sum'n'count x =
        let 
            helper 0 0 0 = (0, 1)
            helper 0 currentSum currentLength = (currentSum, currentLength)
            helper x currentSum currentLength = helper (quot x 10) (currentSum + abs (rem x 10)) (currentLength + 1)
        in 
            helper x 0 0
        
    sum'n'count2 x = 
        helper 0 0 (abs x)
        where
            helper 0 0 0 = (0, 1)
            helper currentSum currentValue x
                | x == 0 = (currentSum, currentValue)
                | otherwise = helper (currentSum + rem x 10) (currentValue + 1) (div x 10)
    
    -- TODO: FIND OUT INTEGRATION
    -- integration f a b = 
    --     helper (f a) (f (a + 1)) 0 b deltax
    --     where
    --         deltax = ()
