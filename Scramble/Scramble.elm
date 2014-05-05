module Scramble.Main where

import Graphics.Input as I
import Set as S
import String

import Scramble.Board (..)
import Scramble.Board as B
import Scramble.Trie (Trie)
import Scramble.Trie as Trie
import Scramble.Utils (..)

debug : Bool
debug = True

minWordLen : Int
minWordLen = 1

-- Main
main = startGame <| startState start5 words

-- Model
type GameState = { board          : Board
                 , curGuess       : [(Position, Char)]
                 , score          : Int
                 , correctGuesses : Trie
                 , response       : Maybe String
                 , dictionary     : Trie
                 , dictTail       : Maybe Trie
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

words : Trie
words = Trie.fromList ["a", "to", "dot", "fan", "vat", "late", "cot", "fib"
                      , "let", "not", "note", "eat", "ate", "pen", "ten"
                      , "con", "cone", "geld", "tan", "if", "bed"
                      ]

startState : Board -> Trie -> GameState
startState b t = { board          = b
                 , curGuess       = []
                 , score          = 0
                 , correctGuesses = Trie.empty
                 , response       = Nothing
                 , dictionary     = t
                 , dictTail       = Just t
                 }

-- Update
interpret : Msg -> GameState -> GameState
interpret m g = case m of
  Tile p c -> { g | curGuess <- tilePress (p, c) g.curGuess
                  , response <- Nothing
                  , dictTail <- Trie.suffixes (expandQ c) =<<? g.dictTail
              }
  Reset    -> { g | curGuess <- []
                  , response <- Nothing
                  , dictTail <- Just g.dictionary
              }
  ChangeBoard b -> { g | board <- b
                       , response <- Just "New game"
                       , score <- 0
                       , dictTail <- Just g.dictionary
                   }
  Guess    ->
      let guess = extractGuess g
      in
        if Trie.member guess g.correctGuesses
        then { g | response <- Just <| "You already guessed: " ++ guess }
        else 
            case Trie.member "" <~? g.dictTail of
              Just True -> { g | score <- g.score + score guess
                               , correctGuesses <- Trie.insert guess g.correctGuesses
                               , curGuess <- []
                               , response <- Just "Correct!"
                               , dictTail <- Just g.dictionary
                           }
              _         -> { g | response <- Just <| "Invalid word: " ++ guess }

score : String -> Int
score = String.length

squareClick : Position -> Char -> GameState -> GameState
squareClick pos c st = { st | curGuess <- tilePress (pos, c) st.curGuess }

expandQ c = if c == 'q' then ['q', 'u'] else [c]
                  
extractGuess : GameState -> String
extractGuess = String.fromList . foldMap (expandQ . snd) . reverse . .curGuess

squaresInput = I.input Nothing
controlInput = I.input Nothing

-- Display
startGame : GameState -> Signal Element
startGame init = 
  let ctrlBtn m = constant . I.button controlInput.handle (Just m)
      sqBtn   m e = I.button squaresInput.handle (Just m) e
      msgs = merge squaresInput.signal controlInput.signal
      state        = foldp (maybe id interpret) init msgs

      currentGuess = extractGuess <~ state

      debugOut = if debug
                 then [ asText <~ squaresInput.signal
                      , asText <~ state ]
                 else []
  in 
   flow down <~ (combine <|
  [ beside <~ (renderBoard sqBtn <~ state)
            ~ (renderStatus <~ state)
  , ctrlBtn Guess "Guess"
  , ctrlBtn Reset "Clear"
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

renderBoard : (Msg -> String -> Element) -> GameState -> Element
renderBoard but = let mkBut p c =  but (Tile p c) (String.cons c "") in 
  flow down . map (flow right) . map (map (uncurry mkBut)) . B.unB . .board

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
