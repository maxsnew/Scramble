module Scramble.Board (make, unB, rotateR, rotateL, neighbor, hardBoards4, easyBoards4)
       where

import Scramble.Utils (..)

import EnumCheck.Enum (..)

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

type Cube a = { n0 : a
              , n1 : a
              , n2 : a
              , n3 : a
              , n4 : a
              , n5 : a
              }

toSides : Cube a -> [a]
toSides c = [c.n0, c.n1, c.n2, c.n3, c.n4, c.n5]

rolls : Cube a -> Enum a
rolls = finE . toSides

boards : [Cube Char] -> Enum Board
boards = mapE (make . map toSides) . finE . perms

fromString : String -> Cube Char
fromString s = let [a,b,c,d,e,f] = String.toList s
               in { n0 = a
                  , n1 = b
                  , n2 = c
                  , n3 = d
                  , n4 = e
                  , n5 = f
                  }

easyBoards4 = boards easy4
easy4 = map fromString [ "AAEEGN"
                       , "ELRTTY"
                       , "AOOTTW"
                       , "ABBJOO"
                       , "EHRTVW"
                       , "CIMOTU"
                       , "DISTTY"
                       , "EIOSST"
                       , "DELRVY"
                       , "ACHOPS"
                       , "HIMNQU"
                       , "EEINSU"
                       , "EEGHNW"
                       , "AFFKPS"
                       , "HLNNRZ"
                       , "DEILRX"
                       ]

hardBoards4 = boards hard4
hard4 = map fromString [ "AACIOT"
                       , "AHMORS"
                       , "EGKLUY"
                       , "ABILTY"
                       , "ACDEMP"
                       , "EGINTV"
                       , "GILRUW"
                       , "ELPSTU"
                       , "DENOSW"
                       , "ACELRS"
                       , "ABJMOQ"
                       , "EEFHIY"
                       , "EHINPS"
                       , "DKNOTU"
                       , "ADENVZ"
                       , "BIFORX"
                       ]
