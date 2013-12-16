module Main where

import open Board
import Board as B
import Graphics.Input as GI
import Input as I
import String

-- Main
main = startGame <| startState start5

-- Model
type GameState = { board    : Board
                 , curGuess : [(Position, Char)] }

data Rot = Clock | CtrClock           

data Msg = Tile Position Char
         | Reset
         | Rotate Rot

-- Initial
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

startState b = { board = b, curGuess = []}             

-- Update
plainButton = fst . GI.button

interpret : Msg -> GameState -> GameState
interpret m g = case m of
  Tile p c -> squareClick p c g
  Reset    -> { g | curGuess <- [] }
  Rotate r -> case r of
    Clock    -> { g | board <- rotateR g.board }
    CtrClock -> { g | board <- rotateL g.board }

squareClick : Position -> Char -> GameState -> GameState
squareClick pos c st = { st | curGuess <- tilePress (pos, c) st.curGuess }

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
  flow down . map (flow right) . map (map (uncurry mkBut)) . B.unB . .board

tileButton : Char -> Element
tileButton c = let s = 50 in 
  collage s s [ toForm . plainText . singleton <| c
              , outlined (solid black) (square s)
              ]

-- Utility
neighbor : Position -> Position -> Bool
neighbor p1 p2 = abs (p1.x - p2.x) == 1 && abs (p1.y - p2.y) == 1
  
member x xs = case xs of
  []        -> False
  (y :: ys) -> if (x == y)
               then True
               else member x ys

tilePress : (Position, Char) -> [(Position, Char)] -> [(Position, Char)]
tilePress p ps = case ps of
  []         -> [p]
  p' :: _  -> if ((neighbor `on` fst) p p' && member (fst p) (map fst ps))
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
