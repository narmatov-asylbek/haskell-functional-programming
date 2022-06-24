module HighOrder where
    import Prelude hiding (filter, takeWhile, dropWhile, span, break, map, concat, concatMap, zipwith)
    import Data.Char (isDigit, isUpper)

    filter :: (a -> Bool) -> [a] -> [a]
    filter predicate [] = []
    filter predicate (x : xs)
        | predicate x = x : filter predicate xs
        | otherwise = filter predicate xs


    takeWhile :: (a -> Bool) -> [a] -> [a]
    takeWhile _ [] = []
    takeWhile pred (x : xs)
        | pred x = x : takeWhile pred xs
        | otherwise = []


    dropWhile :: (a -> Bool) -> [a] -> [a]
    dropWhile _ [] = []
    dropWhile pred xs@(x : xs')
        | pred x = dropWhile pred xs'
        | otherwise = xs


    span :: (a -> Bool) -> [a] -> ([a], [a])
    span p xs = (takeWhile p xs, dropWhile p xs)


    break :: (a -> Bool) -> [a] -> ([a], [a])
    break p = span (not . p)

    readDigits :: String -> (String, String)
    readDigits = span isDigit


    filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
    -- filterDisj _ _ [] = []
    -- filterDisj f1 f2 (x : xs) = 
    --     let
    --         isAny x = f1 x || f2 x
    --     in
    --         if isAny x then x : filterDisj f1 f2 xs else filterDisj f1 f2 xs

    filterDisj f1 f2 = filter (\x -> f1 x || f2 x)

    qsort :: Ord a => [a] -> [a]
    qsort [] = []
    qsort (x : xs) =
        let
            lesserThan xs = filter (<= x) xs
            greaterThan xs = filter (> x) xs
        in
            qsort (lesserThan xs) ++ [x] ++ qsort (greaterThan xs)


    map :: (a -> b) -> [a] -> [b]
    map _ [] = []
    map f (x : xs) = f x : map f xs

    concat :: [[a]] -> [a]
    concat [] = []
    concat (xs: xss) = xs ++ concat xss


    concatMap :: (a -> [b]) -> [a] -> [b]
    concatMap f = concat . map f

    squares'n'cubes :: Num a => [a] -> [a]
    squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])

    delAllUpper :: String -> String
    delAllUpper = unwords . filter (not . all isUpper) . words

    zipwith :: (a -> b -> c) -> [a] -> [b] -> [c]
    zipwith _ [] _ = []
    zipwith _ _ [] = []
    zipwith f (x : xs) (y : ys) = f x y : zipwith f xs ys
