module Board (make, unB, rotateR, rotateL)
       where

data Board = B [[(Position, Char)]]
-- Boards are assumed to be nxn for some n
type Position = { x : Int, y : Int }

unB : Board -> [[(Position, Char)]]
unB (B b) = b

tiles : Board -> [[Char]]
tiles = map (map snd) . unB

make : [[Char]] -> Board
make b = let l = length b
             ns = [0..(l - 1)]
             mkP m n = { x = n, y = m }
         in B <| zipWith (\n cs -> zipWith (\m c -> (mkP n m, c)) ns cs) ns b

rotateR : Board -> Board
rotateR (B b) = let base = repeat (length b) []
                in  B . foldl (zipWith (::)) base <| b

rotateL : Board -> Board
rotateL (B b) = let base = repeat (length b) []
                in B . foldr (zipWith (::)) base . map reverse <| b
