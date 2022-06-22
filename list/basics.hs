module Basics where
    emptyList = []

    notEmptyList = 3 : emptyList

    myLst = 5 : 3 : []


    nTimes :: a -> Int -> [a]
    nTimes a counter
        | counter <= 0 = []
        | otherwise = a : nTimes a (counter - 1)

    firstElement = head myLst
    otherElement = tail myLst
    secondElement = head . tail

    customSecond [] = []
    customSecond [x] = error "Only on element"
    customSecond (_ : x : _ ) = x


    sndHead = snd . head


    customLength :: [a] -> Int
    customLength [] = 0
    customLength (_ : xs) = 1 + customLength xs


    customConcat :: [a] -> [a] -> [a]
    customConcat [] ys = ys
    customConcat (x : xs) ys = x : customConcat xs ys


    oddsOnly :: Integral a => [a] -> [a]
    -- oddsOnly [] = []
    -- oddsOnly (x : xs) = if odd x then x : oddsOnly xs else oddsOnly xs

    oddsOnly = filter odd


    customLast :: [a] -> a
    customLast [] = error "Empty List"
    customLast [x] = x
    customLast (_ : xs) = customLast xs


    withoutLast :: [a] -> [a]
    withoutLast [] = []
    withoutLast [_] = []
    withoutLast (x : xs) = x : withoutLast xs


    customMax :: (Ord a) => [a] -> a
    customMin :: (Ord a) => [a] -> a
    customSum :: (Num a) => [a] -> a
    customProduct :: (Num a) => [a] -> a

    customSum [] = 0
    customSum(x : xs) = x + customSum xs

    customProduct [] = 1
    customProduct (x : xs) = x * customProduct xs

    customMax [] = error "Empty elements"
    customMax (x : xs) = findMax x xs
        where
            findMax current [] = current
            findMax current (x : xs) = if x > current then findMax x xs else findMax current xs

    customMin [] = error "Empty Fields"
    customMin (x : xs) = findMin x xs
        where
            findMin current [] = current
            findMin current (x : xs) = if x < current then findMin x xs else findMin current xs



    customReverse :: [a] -> [a]
    customReverse [] = []
    customReverse xs =
        let
            iter [] init = init
            iter (x : xs) init = iter xs (x : init)
        in
            iter xs []


    isPalindrome :: Eq a => [a] -> Bool
    isPalindrome [] = True
    isPalindrome xs = isEqual xs (reverse xs)
        where
            isEqual [] [] = True
            isEqual (x : xs) (y : ys) = (x == y) && isEqual xs ys
            isEqual _ _ = False

    

    customZip :: [a] -> [b] -> [(a, b)]
    customZip [] _ = []
    customZip _ [] = []
    customZip (x : xs) (y : ys) = (x, y) : customZip xs ys


    customUnzip :: [(a, b)] -> ([a], [b])
    customUnzip [] = ([], [])
    customUnzip ((x, y) : xys) = 
        let (xs, ys) = customUnzip xys
        in
            (x : xs, y : ys)
