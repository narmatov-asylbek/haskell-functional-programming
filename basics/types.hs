module TypeClasses where


-- class Eq a where
--     --- (===), (/==) :: a -> a -> Bool
--     (===) :: a -> a -> Bool
--     (/==) :: a -> a -> Bool
--     x /== y = not (x === y)


-- instance TypeClasses.Eq Bool where
--     True === True = True
--     False === False = True
--     _ === _ = False

-- instance (TypeClasses.Eq a, TypeClasses.Eq b) => TypeClasses.Eq(a, b) where
--     p1 === p2 = fst p1 === fst p2 && snd p1 === snd p2


class Printable a where
    toString :: a -> [Char]


instance Printable Bool where
    toString True = "true"
    toString False = "false"


instance Printable () where
    toString () = "unit type"


instance (Printable a, Printable b) => Printable(a, b) where
    toString (p1, p2) = "(" ++ toString p1 ++ "," ++ toString p2 ++ ")"


class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a
      | doesEnrageMork a && doesEnrageGork a = stomp $ stab a
      | doesEnrageGork a = stab a
      | doesEnrageMork a = stomp a
      | otherwise = a


mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (x: xs) = f x : mapList  f xs
result = mapList (\x -> x + 1) [1, 2, 4]


-- class CustomEnum a where
--     succ, pred :: a -> a
--     toEnum :: Int -> a
--     fromEnum :: a -> Int

class (Eq a, Enum a, Bounded a) => SafeEnum a where
    ssucc :: a -> a
    spred :: a -> a

    ssucc a
        | a == maxBound = minBound
        | otherwise = succ  a


    spred a
        | a == minBound = maxBound
        | otherwise = pred a


instance SafeEnum Int where

instance SafeEnum Bool where

instance SafeEnum Char where



avg :: Int -> Int -> Int -> Double
avg x y z = result x y z where
    sum = toInteger  x + toInteger y + toInteger z
    result x y z = fromInteger sum / 3