module Main where

import Graphics.Input as GI
import Input as I
import String

-- Main
main = startGame <| startState start5

-- Boards are assumed to be nxn for some n
type Board = [[Char]]
type Position = { x : Int, y : Int }

-- Model
type GameState = { board    : Board
                 , curGuess : [Position] }

data Rot = Clock | CtrClock           

data Msg = Tile Position Char
         | Reset
         | Rotate Rot

-- Initial
start3 = [ ['a', 'b', 'c']
         , ['d', 'e', 'f']
         , ['g', 'h', 'i'] ]

start5 = [ ['t', 'd', 'o', 'a', 'f']
         , ['l', 'n', 't', 'c', 'n']
         , ['x', 'e', 't', 'p', 'e']
         , ['f', 'g', 'l', 't', 'v']
         , ['f', 'd', 'e', 'a', 't'] ]

startState b = { board = b, curGuess = []}             

-- Update
plainButton = fst . GI.button

rotateR : [[a]] -> [[a]]
rotateR b = let base = repeat (length b) []
            in foldl (zipWith (::)) base b

rotateL b = let base = repeat (length b) []
            in foldr (zipWith (::)) base . map reverse <| b

interpret : Msg -> GameState -> GameState
interpret m g = case m of
  Tile p c -> squareClick p c g
  Reset    -> { g | curGuess <- [] }
  Rotate r -> case r of
    Clock    -> { g | board <- rotateR g.board }
    CtrClock -> { g | board <- rotateL g.board }

squareClick : Position -> Char -> GameState -> GameState
squareClick pos c st = { st | curGuess <- consNew pos st.curGuess }

-- Display
startGame : GameState -> Signal Element
startGame init = 
  let btns         = I.buttons Nothing
      button m e = btns.button (Just m) e
      state        = foldp (maybe id interpret) init (btns.events)
      pButton m s = constant . button m . plainButton <| s
  in
   flow down <~ combine [ renderBoard button <~ state
                        , pButton Reset          "Reset"
                        , pButton (Rotate Clock) "Clockwise"
                        , pButton (Rotate CtrClock) "Counter-Clockwise"
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
member x xs = case xs of
  []        -> False
  (y :: ys) -> if (x == y)
               then True
               else member x ys

consNew : Position -> [Position] -> [Position]
consNew p ps = if member p ps
               then ps
               else p :: ps

singleton : Char -> String
singleton c = String.cons c ""

mapWithIndex : (Position -> a -> b) -> [[a]] -> [[b]]
mapWithIndex f bd = 
  let max = length bd - 1
      inner x cs = zipWith (\y c -> f {x = x, y = y} c) [0..max] cs in
  zipWith inner [0..max] bd


