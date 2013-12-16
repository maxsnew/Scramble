module Input (buttons)
       where
import Graphics.Input as I
import Mouse

const : a -> b -> a
const x y = x

buttons : a -> { events    : Signal a
               , button : a -> Element -> Element }
buttons def = let hovs = I.hoverables def
              in { events    = keepWhen Mouse.isDown def hovs.events
                 , button = \v -> hovs.hoverable (const v)
                 }

scs = buttons Nothing

ele n = scs.button (Just n) (asText n)
celm = constant . ele

main = flow down <~ (combine <|
       [ celm 1
       , celm 2
       , celm 3
       , asText <~ scs.events 
       ])
