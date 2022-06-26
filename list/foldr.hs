module Foldr where
    import Prelude hiding (foldl, foldr)

    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr _ ini [] = ini
    foldr f ini (x : xs) = x `f` foldr f ini xs

    sumList :: [Integer] -> Integer
    sumList = foldr (+) 0