module Utils (member, remove, (>>=), foldMap)
       where

member : a -> [a] -> Bool
member x = foldr (\y b -> b || x == y) False

remove x xs = case xs of
  []        -> []
  (y :: ys) -> if x == y
               then ys
               else y :: (remove x ys)

(>>=) : [a] -> (a -> [b]) -> [b]
xs >>= k = foldr ((++) . k) [] xs

join : [[a]] -> [a]
join = foldr (++) []

foldMap : (a -> [b]) -> [a] -> [b]
foldMap f = join . map f

main = asText <|
       [1,2,3] >>= \x ->
       [4,5,6] >>= \y ->
       [(x,y)]
