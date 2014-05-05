module Scramble.Utils (member, remove, (>>=), foldMap, (?>>=))
       where

member : a -> [a] -> Bool
member x = foldr (\y b -> b || x == y) False

remove x xs = case xs of
  []        -> []
  (y :: ys) -> if x == y
               then ys
               else y :: (remove x ys)

-- List monad ops
(>>=) : [a] -> (a -> [b]) -> [b]
xs >>= k = foldr ((++) . k) [] xs

join : [[a]] -> [a]
join = foldr (++) []

foldMap : (a -> [b]) -> [a] -> [b]
foldMap f = join . map f

-- Maybe monad
(?>>=) : Maybe a -> (a -> Maybe b) -> Maybe b
mx ?>>= k =
    case mx of
      Nothing -> Nothing
      Just x  -> k x

(=<<?) : (a -> Maybe b) -> Maybe a -> Maybe b
(=<<?) = flip (?>>=)

(<~?) : (a -> b) -> Maybe a -> Maybe b
f <~? m = (Just . f) =<<? m