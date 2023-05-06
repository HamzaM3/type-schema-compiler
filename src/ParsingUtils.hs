module ParsingUtils where

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (a -> c) -> (b, a) -> (b, c)
mapSnd f (a, b) = (a, f b)

type a :+: b = Either a b

enum :: (Enum a, Bounded a) => [a]
enum = enumFrom (toEnum 0)

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf f x = if f x then Nothing else Just x
