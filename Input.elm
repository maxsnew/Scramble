module Input (buttons)
       where
import Graphics.Input as I
import Mouse
import Maybe as M

const : a -> b -> a
const x y = x

-- Infinite loop bc I don't know how to do errors
fromJust x = case x of
  Just y -> y
  Nothing -> fromJust x

justs : a -> Signal (Maybe a) -> Signal a
justs x s = fromJust <~ (keepIf M.isJust (Just x) s)

buttons : a -> { events    : Signal a
               , button    : a -> Element -> Element }
buttons def = let hovs = I.hoverables (Just def)
              in { events = justs def . keepWhen Mouse.isDown (Just def) <| hovs.events
                 , button = \v -> hovs.hoverable (\b -> if b
                                                        then Just v
                                                        else Nothing)
                 }

-- Test
scs = buttons Nothing

ele n = scs.button (Just n) (asText n)
celm = constant . ele

main = flow down <~ (combine <|
       [ celm 1
       , celm 2
       , celm 3
       , asText <~ scs.events 
       ])
