module Scramble.Trie where

import Dict as D
import String as S

import Scramble.Parser ((>>$), (<|>), (>>=$), expect, eof, sepBy, lowerAlpha, parens)
import Scramble.Parser as P
import Scramble.Utils (..)
    
data Trie = Node Bool (D.Dict Char Trie)

empty : Trie
empty = Node False D.empty

insert : String -> Trie -> Trie
insert = insert' . S.toList

insert' : [Char] -> Trie -> Trie
insert' cs (Node b d) = case cs of 
  []        -> Node True d
  (c :: cs') -> let updater m = Just <| case m of
                      Nothing -> insert' cs' empty
                      Just t' -> insert' cs' t'
                in Node b . D.update c updater <| d

toList : Trie -> [String]
toList (Node b d) = 
    let curWord = if b then [""] else []
        restWords =
            D.toList d >>= \(c, t) ->
             map (String.cons c) (toList t)
    in curWord ++ restWords
    
fromList : [String] -> Trie
fromList = foldr insert empty

member : String -> Trie -> Bool
member = member' . S.toList

member' : [Char] -> Trie -> Bool
member' cs (Node b d) = case cs of
  []        -> b
  (c :: cs) -> maybe False (member' cs) <| D.get c d

suffixes : [Char] -> Trie -> Maybe Trie
suffixes cs t = foldr (\c mt -> suffixes' c =<<? mt) (Just t) cs

suffixes' : Char -> Trie -> Maybe Trie
suffixes' c (Node _ d) = D.get c d

-- | Binary encode/decode
encode : Trie -> String
encode (Node b d) = 
    let bbit = if b then "1" else "0"
        encPair (c, t) = String.concat [String.fromList [c], encode t , ";"]
        dbit = String.concat . intersperse "," . map encPair <| D.toList d
    in String.concat [bbit, dbit]

decode : String -> Maybe Trie
decode = P.run trieP 0

trieP : P.Parser st Trie
trieP st = ((Node `P.map` bitP) `P.ap` dictP) st

emptyP : P.Parser st Trie
emptyP st = (eof >>$ P.pure empty) st

bitP : P.Parser st Bool
bitP st = (P.char >>=$ \c ->
               case c of
                 '0' -> P.pure False
                 '1' -> P.pure True
                 _   -> P.fail) st

dictP : P.Parser st (D.Dict Char Trie)
dictP st = (D.fromList `P.map` sepBy (entryP) (expect ',')) st

-- entryP' : () -> P.Parser st (Char, Trie)
entryP st = (lowerAlpha   >>=$ \c ->
             trieP        >>=$ \t ->
             expect ';'   >>$
             P.pure (c, t)) st

start = fromList ["what", "who", "where", "when", "how", "hoot", "huff"]
fromJust (Just x) = x
res = fromJust . decode . encode  <| start
-- main = asText (start == start)
