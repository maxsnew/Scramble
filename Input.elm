module Input (clickables)
       where
import Graphics.Input as I
import Mouse

const : a -> b -> a
const x y = x

clickables : a -> { events    : Signal a
                  , clickable : a -> Element -> Element }
clickables def = let hovs = I.hoverables def
                 in { events    = keepWhen Mouse.isDown def hovs.events
                    , clickable = \v -> hovs.hoverable (const v)
                    }

scs = clickables Nothing

ele n = scs.clickable (Just n) (asText n)
celm = constant . ele

main = flow down <~ (combine <|
       [ celm 1
       , celm 2
       , celm 3
       , asText <~ scs.events 
       ])
