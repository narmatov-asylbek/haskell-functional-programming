module TypeClasses where


class Eq a where
    --- (===), (/==) :: a -> a -> Bool
    (===) :: a -> a -> Bool
    (/==) :: a -> a -> Bool



instance TypeClasses.Eq Bool where
    True === True = True
    False === False = True
    _ === _ = False
    x /== y = not (x === y)
