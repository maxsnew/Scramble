module Trie where

import Dict as D
import String as S

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

fromList : [String] -> Trie
fromList = foldr insert empty

member : String -> Trie -> Bool
member = member' . S.toList

member' : [Char] -> Trie -> Bool
member' cs (Node b d) = case cs of
  []        -> b
  (c :: cs) -> maybe False (member' cs) <| D.lookup c d

main = asText . member "a" . insert "a" <| empty
