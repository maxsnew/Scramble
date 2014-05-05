module Scramble.Board (make, unB, rotateR, rotateL, neighbor)
       where

import Utils (..)

data Board = B [[(Position, Char)]]
-- Boards are assumed to be nxn for some n
type Position = { x : Int, y : Int }

cX : (Int -> Int) -> Position -> Position
cX f p = { p | x <- f p.x }
cY : (Int -> Int) -> Position -> Position
cY f p = { p | y <- f p.y }

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

neighbor : Position -> Position -> Bool
neighbor p1 p2 = (p1 /= p2) && (member p2 <| neighbors p1)

neighbors : Position -> [Position]
neighbors p = let add1 n = n + 1
                  sub1 n =  n - 1
                  trans  = [add1, sub1, id]
                  transX = map cX trans
                  transY = map cY trans
              in transX >>= \t1 ->
              transY >>= \t2 ->
              [t1 . t2 <| p]
