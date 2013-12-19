module Main where

import Graphics.Input as GI
import Set as S

import open Board
import Board as B
import Input as I
import String

debug : Bool
debug = True

-- Main
main = startGame <| startState start3

-- Model
type GameState = { board          : Board
                 , curGuess       : [(Position, Char)]
                 , score          : Int
                 , correctGuesses : [String]
                 }

data Msg = Tile Position Char
         | Reset
         | ChangeBoard Board

-- Initial
start2 = B.make <| [ ['a', 'b']
                   , ['c', 'd'] ]
start3 = B.make <|
         [ ['a', 'b', 'c']
         , ['d', 'e', 'f']
         , ['g', 'h', 'i'] ]

start5 = B.make <|
         [ ['t', 'd', 'o', 'a', 'f']
         , ['l', 'n', 't', 'c', 'n']
         , ['x', 'e', 't', 'p', 'e']
         , ['f', 'g', 'l', 't', 'v']
         , ['f', 'd', 'e', 'a', 't'] ]

words : S.Set String
words = S.fromList ["a", "to", "dot", "fan", "vat", "late", "cot", "fib"
                   , "let", "not", "note", "eat", "ate", "pen", "ten"
                   , "con", "cone", "geld", "tan"
                   ]

startState : Board -> GameState
startState b = { board = b
               , curGuess = []
               , score = 0
               , correctGuesses = []
               }

-- Update
interpret : Msg -> GameState -> GameState
interpret m g = case m of
  Tile p c -> squareClick p c g
  Reset    -> { g | curGuess <- [] }
  ChangeBoard b -> { g | board <- b }

squareClick : Position -> Char -> GameState -> GameState
squareClick pos c st = { st | curGuess <- tilePress (pos, c) st.curGuess }

-- Display
startGame : GameState -> Signal Element
startGame init = 
  let btns  = I.buttons Nothing
      pbtns = GI.buttons Nothing
      pButton m = constant . pbtns.button (Just m)
      button m e = btns.button (Just m) e
      msgs = merge btns.events pbtns.events
      state        = foldp (maybe id interpret) init msgs

      currentGuess = String.fromList . reverse . map snd . .curGuess <~ state

      debugOut = if debug
                 then [ asText <~ btns.events , asText <~ state ]
                 else []
  in 
   flow down <~ (combine <|
  [ beside <~ (renderBoard button <~ state)
            ~ (plainText <~ (String.append "Current Guess: " <~ currentGuess))
  , pButton Reset                "Reset"
  , pButton (ChangeBoard start2) "Change 2"
  , pButton (ChangeBoard start3) "Change 3"
  , pButton (ChangeBoard start5) "Change 5"
  ]
  ++
  debugOut)
                       
renderBoard : (Msg -> Element -> Element) -> GameState -> Element
renderBoard but = let mkBut p c =  but (Tile p c) (tileButton c) in 
  flow down . map (flow right) . map (map (uncurry mkBut)) . B.unB . .board

tileButton : Char -> Element
tileButton c = let s = 50 in 
  collage s s [ toForm . plainText . singleton <| c
              , outlined (solid black) (square (s / 2))
              ]

-- Utility
neighbor : Position -> Position -> Bool
neighbor p1 p2 = True

member x xs = case xs of
  []        -> False
  (y :: ys) -> if (x == y)
               then True
               else member x ys

tilePress : (Position, Char) -> [(Position, Char)] -> [(Position, Char)]
tilePress p ps = case ps of
  []         -> [p]
  p' :: _  -> if ((neighbor `on` fst) p p' && not (member (fst p) (map fst ps)))
              then p :: ps
              else ps

consNew : Position -> [Position] -> [Position]
consNew p ps = if member p ps
               then ps
               else p :: ps

singleton : Char -> String
singleton c = String.cons c ""

on : (a -> a -> c) -> (b -> a) -> (b -> b -> c)
on p f = \x y -> p (f x) (f y)
