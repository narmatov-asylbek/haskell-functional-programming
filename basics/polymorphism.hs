module Some where
import Data.Function

id x = x
k x y = x
applyTwice f x = f $ f x

sumSquares = (+) `on` (^2)
g = (*)
h = snd
multiSecond = g `on` h

p1 = ((1, 2), (2, 3))
p2 = ((3, 4), (3, 4))

sumFstFst = (+) `on` (\pp -> fst $ fst pp)

on3 op f x y z = op (f x) (f y) (f z)

-- f :: b -> c
-- g :: a -> b
-- x :: a

compose f g = \x -> f (g x)
sumFstFst2 = (+) `on` (fst . fst)

hello:: a -> (a, b) -> a -> (b, a, a)
hello a b c = (snd b, a, a)
-- snd b, a, a
-- snd b, a, c
-- snd b, c, c
-- snd b, c, a
-- snd b, fst b, a
-- snd b, fst b, fst b
-- snd b, fst b, c
-- snd b, c, fst b
-- snd b, a, fst b

swap = uncurry (flip (,))
