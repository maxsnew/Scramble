module Main where

import Graphics.Input
import String

type Board = [[Char]]
type Position = { x : Int, y : Int }

type GameState = { board    : Board
                 , curGuess : [Position] }

main = render startState

startBoard : Board
startBoard = [ ['a', 'b']
             , ['c', 'd'] ]

mapWithIndex : (Position -> a -> b) -> [[a]] -> [[b]]
mapWithIndex f bd = 
  let max = length bd
      inner x cs = zipWith (\y c -> f {x = x, y = y} c) [1..max] cs in
  zipWith inner [1..max] bd
  

boardButtons : Board -> { events  : Signal (Maybe Position)
                        , squares : [[ Element ]] }
boardButtons bd =
  let btns = Graphics.Input.buttons Nothing in
  { btns - button |
    squares =  mapWithIndex (\p c -> btns.button (Just p) (String.cons c "")) bd }

startState = { board = startBoard
             , curGuess = []}             


toElement : Board -> Element
toElement = flow down . map (flow right . map asText)

posToPair : Position -> (Int, Int)
posToPair p = (p.x, p.y)

render : GameState -> Element
render st = let btns = boardButtons st.board in
  flow down [ flow down . map (flow right) <| btns.squares
            , asText st.curGuess
            ]



