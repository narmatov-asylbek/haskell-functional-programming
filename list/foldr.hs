module Foldr where
    import Prelude hiding (foldl, foldr)

    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr _ ini [] = ini
    foldr f ini (x : xs) = x `f` foldr f ini xs

    sumList :: [Integer] -> Integer
    sumList = foldr (+) 0

    concatList = foldr (++) []

    sumPositiveSquares :: [Integer] -> Integer
    sumPositiveSquares = foldr f 0 where
        f x s | x > 0 = x ^ 2 + s
              | otherwise = s

    lengthList = foldr (\_ s -> s + 1) 0

    sumOdd = foldr (+) 0 . filter odd

    some = foldr (:) []

    another = foldr const undefined