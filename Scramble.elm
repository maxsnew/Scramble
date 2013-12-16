module Main where

import Input as I
import String

type Board = [[Char]]
type Position = { x : Int, y : Int }

-- Model
type GameState = { board    : Board
                 , curGuess : [Position] }

data Msg = Square Position Char

main = render startState

-- Initial
startBoard : Board
startBoard = [ ['a', 'b', 'c']
             , ['d', 'e', 'f']
             , ['g', 'h', 'i'] ]

startState = { board = startBoard
             , curGuess = []}             

-- boardButtons : Board
--                -> { events  : Signal (Maybe a)
--                   , button  : a -> Element -> Element 
--  }
-- boardButtons bd =
--   let btns = I.clickables Nothing
--       square p c = btns.button (Just (p, c))
--   in { btns - clickable | button = square }

-- Update
interpret : Msg -> GameState -> GameState
interpret m = case m of
  Square p c -> squareClick p c

squareClick : Position -> Char -> GameState -> GameState
squareClick pos c st = { st | curGuess <- consNew pos st.curGuess }

member x xs = case xs of
  []        -> False
  (y :: ys) -> if (x == y)
               then True
               else member x ys

consNew : Position -> [Position] -> [Position]
consNew p ps = if member p ps
               then ps
               else p :: ps

-- Display
render : GameState -> Signal Element
render init = let btns = I.buttons Nothing
                  state = foldp (maybe id interpret) init (btns.events)
              in
  flow down <~ combine [ renderBoard btns.button <~ state
                       , asText <~ btns.events
                       , asText <~ state]

renderBoard : (Maybe Msg -> Element -> Element) -> GameState -> Element
renderBoard but = let mkBut p c =  but (Just (Square p c)) (renderButton c) in 
  flow down . map (flow right) . mapWithIndex mkBut . .board

renderButton : Char -> Element
renderButton c = collage 50 50 [ toForm . plainText . singleton <| c
                               , outlined (solid black) (square 25)
                               ]

-- Utility
singleton : Char -> String
singleton c = String.cons c ""

mapWithIndex : (Position -> a -> b) -> [[a]] -> [[b]]
mapWithIndex f bd = 
  let max = length bd - 1
      inner x cs = zipWith (\y c -> f {x = x, y = y} c) [0..max] cs in
  zipWith inner [0..max] bd

maybeMap : (a -> b) -> Maybe a -> Maybe b
maybeMap f = maybe Nothing (Just . f)
