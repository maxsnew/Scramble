module Main where

import Graphics.Input as GI
import Set as S
import String

import open Board
import Board as B
import Input as I
import open Utils

debug : Bool
debug = True

minWordLen : Int
minWordLen = 1

-- Main
main = startGame <| startState start3

-- Model
type GameState = { board          : Board
                 , curGuess       : [(Position, Char)]
                 , score          : Int
                 , correctGuesses : [String]
                 , response       : Maybe String
                 }

data Msg = Tile Position Char
         | Reset
         | ChangeBoard Board
         | Guess

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
                   , "con", "cone", "geld", "tan", "if", "bed"
                   ]

startState : Board -> GameState
startState b = { board          = b
               , curGuess       = []
               , score          = 0
               , correctGuesses = []
               , response       = Nothing
               }

-- Update
interpret : Msg -> GameState -> GameState
interpret m g = case m of
  Tile p c -> { g | curGuess <- tilePress (p, c) g.curGuess
                  , response <- Nothing }
  Reset    -> { g | curGuess <- [], response <- Nothing }
  ChangeBoard b -> { g | board <- b
                       , response <- Just "New game"
                       , score <- 0 }
  Guess    -> let guess = extractGuess g
              in if (not . String.isEmpty <| guess) && guess `S.member` words
                 then { g | score <- g.score + score guess
                          , correctGuesses <- guess :: g.correctGuesses
                          , curGuess <- []
                          , response <- Just "Correct!"
                      }
                 else { g | response <- Just <| "Invalid word: " ++ guess }

score : String -> Int
score = String.length


squareClick : Position -> Char -> GameState -> GameState
squareClick pos c st = { st | curGuess <- tilePress (pos, c) st.curGuess }

expandQ c = if c == 'q' then ['q', 'u'] else [c]
                  
extractGuess : GameState -> String
extractGuess = String.fromList . foldMap (expandQ . snd) . reverse . .curGuess

-- Display
startGame : GameState -> Signal Element
startGame init = 
  let btns  = I.buttons Nothing
      pbtns = GI.buttons Nothing
      pButton m = constant . pbtns.button (Just m)
      button m e = btns.button (Just m) e
      msgs = merge btns.events pbtns.events
      state        = foldp (maybe id interpret) init msgs

      currentGuess = extractGuess <~ state

      debugOut = if debug
                 then [ asText <~ btns.events
                      , asText <~ state ]
                 else []
  in 
   flow down <~ (combine <|
  [ beside <~ (renderBoard button <~ state)
            ~ (renderStatus <~ state)
  , pButton Guess "Guess"
  , pButton Reset "Clear"
  ]
  ++
  debugOut)

toList : Maybe a -> [a]
toList = maybe [] (\x -> [x])

renderStatus : GameState -> Element
renderStatus g = let withPre s = plainText . String.append s in 
  flow down <|
  [ (withPre "Current Guess: " . extractGuess <| g)
  , (withPre "Score: " . show . .score <| g)
  ] ++ (map plainText . toList . .response <| g)

renderBoard : (Msg -> Element -> Element) -> GameState -> Element
renderBoard but = let mkBut p c =  but (Tile p c) (tileButton c) in 
  flow down . map (flow right) . map (map (uncurry mkBut)) . B.unB . .board

tileButton : Char -> Element
tileButton c = let s = 50 in 
  collage s s [ toForm . plainText . singleton <| c
              , outlined (solid black) (square (s / 2))
              ]

-- Utility
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
