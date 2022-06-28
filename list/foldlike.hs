module FoldLike where
    import Prelude hiding(foldl1, foldr1, scanl, unfold)
    
    foldr1 :: (a -> a -> a) -> [a] -> a
    foldr1 _ [x] = x
    foldr1 f (x : xs) = f x (foldr1 f xs)
    foldr1 _ [] = error "foldr: Empty List"

    foldl1 :: (a -> a -> a) -> [a] -> a
    foldl1 f (x : xs) = foldl f x xs
    foldl1 _ [] = error "foldl1: Empty List"

    maximum :: (Ord a) => [a] -> a
    maximum  = foldl1 max

    lastElem :: [a] -> a
    lastElem = foldl1 (\x y -> y)

    scanl :: (b -> a -> b) -> b -> [a] -> [b]
    scanl _ ini [] = [ini]
    scanl f ini (x : xs) = ini : scanl f (ini `f` x) xs

    unfold ::(b -> (a, b)) -> b -> [a]
    unfold f ini = let (x, ini') = f ini in
        x : unfold f ini'