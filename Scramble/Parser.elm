module Scramble.Parser where

import Char (isLower)
import String

peek : PState st -> Maybe Char
peek st = 
    let n = st.cursor
        s = st.src
    in case String.uncons (String.slice n (n+1) s) of
         Nothing -> Nothing
         Just (c, _)  -> Just c

(+=) : PState st -> Int -> PState st
st += n = { st | cursor <- st.cursor + n }

type PState st = { cursor : Int
                 , src : String
                 , state : st
                 }
data Result st a = Done a (PState st)
                 | Fail

type Parser st a = (PState st) -> Result st a

run : Parser st a -> st -> String -> Maybe a
run p st s = case p { cursor = 0 , src = s , state = st } of
               Fail -> Nothing
               Done x _ -> Just x

fail : Parser st a
fail _ = Fail

pure : a -> Parser st a
pure = Done

char : Parser st Char
char st = case peek st of
            Nothing -> Fail
            Just c  -> Done c (st += 1)

lowerAlpha : Parser st Char
lowerAlpha = char >>=$ \c ->
             if isLower c 
             then pure c
             else fail

expect : Char -> Parser st Char
expect c = char >>=$ \c2 -> 
           if c == c2
           then pure c2
           else fail

eof : Parser st ()
eof st = case peek st of
           Nothing -> Done () st
           _       -> Fail

mapRes : (a -> b) -> Result st a -> Result st b
mapRes f res = case res of
                 Done x st -> Done (f x) st
                 Fail -> Fail

map : (a -> b) -> Parser st a -> Parser st b
map f p = mapRes f . p

ap : Parser st (a -> b) -> Parser st a -> Parser st b
ap p1 p2 = p1 >>=$ \ f -> p2 >>=$ \ x -> pure (f x)

(<|>) : Parser st a -> Parser st a -> Parser st a
p1 <|> p2 = 
    \st ->
        case p1 st of
          Fail -> p2 st
          Done a st' -> Done a st'

-- get : Parser st st
-- get st = Done (st.state) st

-- inc : Parser number ()
-- inc = modify (\n -> n + 1)

-- dec : Parser number ()
-- dec = modify (\n -> n - 1)

-- modify : (st -> st) -> Parser st ()
-- modify f st = Done () { st | state <- f st.state }

(>>=$) : Parser st a -> (a -> Parser st b) -> Parser st b
p >>=$ f = 
    \st ->
        case p st of
          Done a st' -> f a st'
          _          -> Fail

(>>$) : Parser st a -> Parser st b -> Parser st b
p1 >>$ p2 = p1 >>=$ \_ -> p2

parens : Parser st a -> Parser st a
parens p = expect '(' >>$
           p          >>=$ \x ->
           expect ')' >>$
           pure x

many1 : Parser st a -> Parser st [a]
many1 p = p      >>=$ \x -> 
          many p >>=$ \xs ->
          pure (x :: xs)

many  : Parser st a -> Parser st [a]
many p = many1 p <|> pure []

sepBy : Parser st a -> Parser st sep -> Parser st [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 : Parser st a -> Parser st sep -> Parser st [a]
sepBy1 p sep = p >>=$ \x ->
               many (sep >>$ p) >>=$ \xs ->
               pure (x :: xs)

listP = expect '[' >>$
        ((expect '0' >>$ pure 0) `sepBy` (expect ',')) >>=$ \xs ->
        expect ']' >>$
        pure xs
