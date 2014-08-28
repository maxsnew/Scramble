module Main where

import EnumCheck.Enum as Enum
import EnumCheck.Enum (Enum)
import Graphics.Input as I
import Maybe (Maybe(..), maybe)
import Set as S
import String

import Scramble.Board (..)
import Scramble.Board as B
import Scramble.Solver (solve)
import Scramble.Trie (Trie)
import Scramble.Trie as Trie
import Scramble.Utils (..)
import Scramble.EasyWords as Words

debug : Bool
debug = False

minWordLen : Int
minWordLen = 1

-- Main
main = startGame <| startState (Enum.takeE Words.max easyBoards4) 0 Words.words

-- Model
type GameState = { board          : Board
                 , boards         : Enum Board
                 , boardI         : Int
                 , answers        : [String]
                 , curGuess       : [(BPosition, Char)]
                 , score          : Int
                 , correctGuesses : Trie
                 , response       : Maybe String
                 , dictionary     : Trie
                 , dictTail       : Maybe Trie
                 }

data Msg = Tile BPosition Char
         | Reset
         | ChangeBoard Int
         | Guess

startState : Enum Board -> Int -> Trie -> GameState
startState bs i t = 
  let b = Enum.fromNat i bs
  in { boards         = bs
     , board          = Enum.fromNat i bs
     , boardI         = i
     , answers        = Trie.toList <| solve b t
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
  Tile p c -> 
    if not <| validPress (p, c) g.curGuess
    then { g | response <- Just "Invalid Move" }
    else { g | response <- Nothing
             , curGuess <- (p, c) :: g.curGuess
             , dictTail <- Trie.suffixes (expandQ c) =<<? g.dictTail
         }
  Reset    -> { g | curGuess <- []
                  , response <- Nothing
                  , dictTail <- Just g.dictionary
              }
  ChangeBoard i -> 
      let i' = (g.boardI + i) `mod` Words.max
          b  = Enum.fromNat i' g.boards
      in { g | board    <- b
             , boardI   <- i'
             , answers  <- Trie.toList <| solve b g.dictionary
             , response <- Just "New game!"
             , curGuess <- []
             , score    <- 0
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

expandQ : Char -> [Char]
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
                      , renderState <~ state
                      ]
                 else []
  in 
   flow down <~ (combine <|
  [ renderGameNo <~ state
  , beside <~ (renderBoard sqBtn <~ state)
            ~ (renderStatus <~ state)
  , ctrlBtn Guess "Guess"
  , ctrlBtn Reset "Clear"
  , beside <~ ctrlBtn (ChangeBoard -1) "←"
            ~ ctrlBtn (ChangeBoard  1) "→"
  ]
  ++
  debugOut)

toList : Maybe a -> [a]
toList = maybe [] (\x -> [x])

renderGameNo : GameState -> Element
renderGameNo st = plainText <| ("Game #" ++ show (st.boardI+1) ++ " of " ++ show Words.max)

renderStatus : GameState -> Element
renderStatus g = let withPre s = plainText . String.append s in 
  flow down <|
  [ (withPre "Current Guess: " . extractGuess <| g)
  , (withPre "Score: " . show . .score <| g)
  ] ++ (map plainText . toList . .response <| g)

renderBoard : (Msg -> String -> Element) -> GameState -> Element
renderBoard but = let mkBut p c =  but (Tile p c) (String.cons c "") in 
  flow down . map (flow right) . map (map (uncurry mkBut)) . B.unB . .board

-- | For debugging
renderState : GameState -> Element
renderState st = flow down [
                  asText <| st.dictTail
                 ]

-- Utility
validPress : (BPosition, Char) -> [(BPosition, Char)] -> Bool
validPress p ps = 
  case ps of
    []      -> True
    p' :: _ -> (neighbor `on` fst) p p' && not (member (fst p) (map fst ps))
    
consNew : BPosition -> [BPosition] -> [BPosition]
consNew p ps = if member p ps
               then ps
               else p :: ps

singleton : Char -> String
singleton c = String.cons c ""

on : (a -> a -> c) -> (b -> a) -> (b -> b -> c)
on p f = \x y -> p (f x) (f y)
