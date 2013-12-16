module Main where

import Graphics.Input as GI
import Input as I
import String

-- Main
main = startGame startState


type Board = [[Char]]
type Position = { x : Int, y : Int }

-- Model
type GameState = { board    : Board
                 , curGuess : [Position] }

data Msg = Tile Position Char
         | Reset

-- Initial
startBoard : Board
startBoard = [ ['a', 'b', 'c']
             , ['d', 'e', 'f']
             , ['g', 'h', 'i'] ]

startState = { board = startBoard
             , curGuess = []}             

-- Update
plainButton = fst . GI.button

interpret : Msg -> GameState -> GameState
interpret m g = case m of
  Tile p c -> squareClick p c g
  Reset    -> { g | curGuess <- [] }

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
startGame : GameState -> Signal Element
startGame init = 
  let btns         = I.buttons Nothing
      button m e = btns.button (Just m) e
      state        = foldp (maybe id interpret) init (btns.events) in
  flow down <~ combine [ renderBoard button <~ state
                       , constant <| button Reset (plainButton "Reset")
                       , asText <~ state]

renderBoard : (Msg -> Element -> Element) -> GameState -> Element
renderBoard but = let mkBut p c =  but (Tile p c) (tileButton c) in 
  flow down . map (flow right) . mapWithIndex mkBut . .board

tileButton : Char -> Element
tileButton c = let s = 50 in 
  collage s s [ toForm . plainText . singleton <| c
              , outlined (solid black) (square s)
              ]

-- Utility
singleton : Char -> String
singleton c = String.cons c ""

mapWithIndex : (Position -> a -> b) -> [[a]] -> [[b]]
mapWithIndex f bd = 
  let max = length bd - 1
      inner x cs = zipWith (\y c -> f {x = x, y = y} c) [0..max] cs in
  zipWith inner [0..max] bd


