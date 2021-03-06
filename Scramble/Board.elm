module Scramble.Board --(make, width, positions, unB, square, rotateR, rotateL, neighbor, neighbors, hardBoards4, easyBoards4)
       where

import BigInt as BI
import BigInt.Convenient as I
import EnumCheck.Enum   (..)
import EnumCheck.ExtNat (..)
import String

import Scramble.Utils (..)

data Board = B [[(BPosition, Char)]]
-- Boards are assumed to be nxn for some n
type BPosition = { x : Int, y : Int }

cX : (Int -> Int) -> BPosition -> BPosition
cX f p = { p | x <- f p.x }
cY : (Int -> Int) -> BPosition -> BPosition
cY f p = { p | y <- f p.y }

unB : Board -> [[(BPosition, Char)]]
unB (B b) = b

width : Board -> Int
width = length << unB

positions : Board -> [BPosition]
positions = map fst << concat << unB

square : BPosition -> Board -> Char
square p = snd << nth p.x << nth p.y << unB

tiles : Board -> [[Char]]
tiles = map (map snd) << unB

make : [[Char]] -> Board
make b = let l = length b
             ns = [0..(l - 1)]
             mkP m n = { x = n, y = m }
         in B <| zipWith (\n cs -> zipWith (\m c -> (mkP n m, c)) ns cs) ns b

rotateR : Board -> Board
rotateR (B b) = let base = repeat (length b) []
                in  B << foldl (zipWith (::)) base <| b

rotateL : Board -> Board
rotateL (B b) = let base = repeat (length b) []
                in B << foldr (zipWith (::)) base << map reverse <| b

neighbor : BPosition -> BPosition -> Bool
neighbor p1 p2 = (p1 /= p2) && (member p2 <| neighbors p1)

neighbors : BPosition -> [BPosition]
neighbors p = let add1 n = n + 1
                  sub1 n =  n - 1
                  trans  = [add1, sub1, identity]
                  transX = map cX trans
                  transY = map cY trans
              in transX >>= \t1 ->
              transY >>= \t2 ->
              [t1 << t2 <| p]

type Cube a = { n0 : a
              , n1 : a
              , n2 : a
              , n3 : a
              , n4 : a
              , n5 : a
              }

toSides : Cube a -> [a]
toSides c = [c.n0, c.n1, c.n2, c.n3, c.n4, c.n5]

nth : Int -> [a] -> a
nth n xs = case drop n xs of
  []   -> Native.Error.raise "Error: Board.elm: nth: not enough elements in the list!"
  x::_ -> x

matrify : [a] -> [[a]]
matrify xs = 
  let l = BI.flroot << BI.length <| xs
      loop xs rows = 
          case xs of
            [] -> reverse rows
            _  -> let (row, rest) = BI.splitAt l xs
                  in loop rest (row :: rows)
  in loop xs []

boardsE : [Cube Char] -> Enum Board
boardsE cubes = 
  let assemble = zipWith (\choice cube -> nth choice (toSides cube))
      choices = (listE (repeat (length cubes) (takeE I.six smallNatE)))
      toBoard = make << matrify
  in toBoard `mapE` (assemble `mapE` choices `apE` (permsE cubes))

fromString : String -> Cube Char
fromString s = let [a,b,c,d,e,f] = String.toList s
               in { n0 = a
                  , n1 = b
                  , n2 = c
                  , n3 = d
                  , n4 = e
                  , n5 = f
                  }

easyBoards4 = let rawE = boardsE easy4
              in everyE ((toBigInt <| rawE.size) `BI.div` (BI.fromString "1000")) rawE
easy4 = map (fromString << String.toLower) [ "AAEEGN"
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

hardBoards4 = boardsE hard4
hard4 = map (fromString << String.toLower) [ "AACIOT"
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
