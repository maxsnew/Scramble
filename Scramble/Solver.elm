module Scramble.Solver where

import Set
import Set (Set)
import String

import Scramble.Board (Board, BPosition)
import Scramble.Board as B
import Scramble.Trie as Trie
import Scramble.Trie (Trie)
import Scramble.Utils (..)

solve : Board           -- ^ Scramble board
        -> Trie         -- ^ Input Dictionary
        -> Trie
solve b t = Trie.unions << map (\pos -> solveAt b Set.empty pos t) <| B.positions b
            

toTup : BPosition -> (Int, Int)
toTup p = (p.x, p.y)

solveAt : Board 
          -> Set (Int, Int) -- ^ Previously visited positions
          -> BPosition     -- ^ current position
          -> Trie         -- ^ valid words here
          -> Trie
solveAt b visited curP t = 
  let c  = B.square curP b
      wC = String.cons c ""
      t1 = Trie.fromList <| if Trie.member wC t then [wC] else []
      mayT2 = Trie.suffixes' c t
  in case mayT2 of
       Nothing -> t1
       Just t2 ->
           let w = B.width b
               goodNeighbor p = p.x >= 0 && p.x < w && p.y >= 0 && p.y < w && not (toTup p `Set.member` visited)
               goodNeighbors  = filter goodNeighbor << B.neighbors <| curP
               goNext pos = solveAt b (Set.insert (toTup pos) visited) pos t2
               ts = map goNext goodNeighbors
               t' = Trie.unions ts
           in Trie.union t1 (Trie.prefixWith c t')
