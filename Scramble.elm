module Main where

import Input as I
import String

type Board = [[Char]]
type Position = { x : Int, y : Int }

type GameState = { board    : Board
                 , curGuess : [Position] }

main = render startState

startBoard : Board
startBoard = [ ['a', 'b', 'c']
             , ['d', 'e', 'f']
             , ['g', 'h', 'i'] ]

mapWithIndex : (Position -> a -> b) -> [[a]] -> [[b]]
mapWithIndex f bd = 
  let max = length bd - 1
      inner x cs = zipWith (\y c -> f {x = x, y = y} c) [0..max] cs in
  zipWith inner [0..max] bd

boardButtons : Board -> { events  : Signal (Maybe (Position, Char))
                        , squares : (Position -> Char -> Element) -> [[ Element ]] }
boardButtons bd =
  let btns = I.clickables Nothing
      mkButton mkElt p c =
        let elt = mkElt p c in
        btns.clickable (Just (p,c)) elt
      squares mkElt = mapWithIndex (mkButton mkElt) bd

  in
   { btns - clickable | squares = squares }

startState = { board = startBoard
             , curGuess = []}             


toElement : Board -> Element
toElement = flow down . map (flow right . map asText)

posToPair : Position -> (Int, Int)
posToPair p = (p.x, p.y)

renderButton : Position -> Char -> Element
renderButton _ c = plainText . singleton <| c

render : GameState -> Signal Element
render st = let btns = boardButtons st.board in
  flow down <~ combine [ constant <| flow down . map (flow right) <| btns.squares renderButton
                       , constant <| asText st.curGuess
                       , asText <~ btns.events]

singleton : Char -> String
singleton c = String.cons c ""
