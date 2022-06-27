module Foldl where
    import Prelude hiding(foldl, foldl', any)

    -- Not recommended for using
    -- foldl :: (b -> a -> b) -> b -> [a] -> b
    -- foldl _ ini [] = ini
    -- foldl f ini (x : xs) = foldl f (f ini x) xs


    -- Optimized version without thunking
    foldl' :: (b -> a -> b) -> b -> [a] -> b
    foldl' f ini [] = ini
    foldl' f ini (x : xs) = ini' `seq` foldl' f ini' xs where
        ini' = f ini x

    any :: (a -> Bool) -> [a] -> Bool
    any p = foldr(\x b -> p x || b) False

    meanList :: [Double] -> Double
    meanList = (\(x, b) -> b / x) . foldr (\x (myLen, mySum) -> (myLen + 1, mySum + x)) (0, 0)

    -- evenOnly = filter (\(pos, _) -> rem pos 2 == 0) . foldl (\x (elems, pos) -> ((x, pos): elems, pos + 1)) ([], 0)
    
    evenOnly :: [a] -> [a]
    evenOnly xs = map fst . filter (even . snd) $ zip xs [1..(length xs)]