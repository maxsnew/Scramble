module Main where

import Input as I
import String

type Board = [[Char]]
type Position = { x : Int, y : Int }

-- Model
type GameState = { board    : Board
                 , curGuess : [Position] }

main = render startState

-- Initial
startBoard : Board
startBoard = [ ['a', 'b', 'c']
             , ['d', 'e', 'f']
             , ['g', 'h', 'i'] ]

startState = { board = startBoard
             , curGuess = []}             

boardButtons : Board -> { events  : Signal (Maybe (Position, Char))
                        , squares : (Position -> Char -> Element) -> [[ Element ]] }
boardButtons bd =
  let btns = I.clickables Nothing
      mkButton mkElt p c =
        let elt = mkElt p c in
        btns.clickable (Just (p,c)) elt
      squares mkElt = mapWithIndex (mkButton mkElt) bd
  in { btns - clickable | squares = squares }

-- Update
member x xs = case xs of
  []        -> False
  (y :: ys) -> if (x == y)
               then True
               else member x ys

consNew : Position -> [Position] -> [Position]
consNew p ps = if member p ps
               then ps
               else p :: ps

step : (Maybe (Position, Char)) -> GameState -> GameState
step press st = case press of
  Nothing       -> st
  Just (pos, c) -> { st | curGuess <- consNew pos st.curGess }

-- Display
render : GameState -> Signal Element
render init = let btns = boardButtons init.board 
                  state = foldp step init btns.events
              in
  flow down <~ combine [ constant <| flow down . map (flow right) <| btns.squares renderButton
                       , constant <| asText init.curGuess
                       , state]

toElement : Board -> Element
toElement = flow down . map (flow right . map asText)

posToPair : Position -> (Int, Int)
posToPair p = (p.x, p.y)

renderButton : Position -> Char -> Element
renderButton _ c = plainText . singleton <| c

-- Utility
singleton : Char -> String
singleton c = String.cons c ""

mapWithIndex : (Position -> a -> b) -> [[a]] -> [[b]]
mapWithIndex f bd = 
  let max = length bd - 1
      inner x cs = zipWith (\y c -> f {x = x, y = y} c) [0..max] cs in
  zipWith inner [0..max] bd

