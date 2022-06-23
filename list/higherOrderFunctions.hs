module HighOrder where
    import Prelude hiding (filter, takeWhile, dropWhile, span, break)
    import Data.Char (isDigit)
    
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
    