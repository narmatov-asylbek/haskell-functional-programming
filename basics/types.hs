module TypeClasses where


class Eq a where
    --- (===), (/==) :: a -> a -> Bool
    (===) :: a -> a -> Bool
    (/==) :: a -> a -> Bool
    x /== y = not (x === y)


instance TypeClasses.Eq Bool where
    True === True = True
    False === False = True
    _ === _ = False

instance (TypeClasses.Eq a, TypeClasses.Eq b) => TypeClasses.Eq(a, b) where
    p1 === p2 = fst p1 === fst p2 && snd p1 === snd p2


class Printable a where
    toString :: a -> [Char]


instance Printable Bool where
    toString True = "true"
    toString False = "false"


instance Printable () where
    toString () = "unit type"


instance (Printable a, Printable b) => Printable(a, b) where
    toString (p1, p2) = "(" ++ toString p1 ++ "," ++ toString p2 ++ ")"