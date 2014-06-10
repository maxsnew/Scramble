module Main where

import EnumCheck.Enum   (..)
import EnumCheck.ExtNat (..)
import IO.IO (..)

import Scramble.Board (..)
import Scramble.Solver (solve)
import Scramble.Trie as Trie
import Scramble.Trie (Trie)
import Scramble.Words (words)

console : IO ()
console = putStrLn "Go!" >>= 
          \_ -> loop 0 Trie.empty >>
          putStrLn "\7"

modulus : Int
modulus = 100

board = easyBoards4
max : Int
max = toInt board.size

loop : Int -> Trie -> IO ()
loop n t = 
    if | n >= max -> log n t
       | otherwise ->
           let mayLog t = if | n `mod` modulus == 0 -> log n t
                             | otherwise            -> pure ()
               b    = fromNat n board
               sols = solve b words
               t'   = Trie.union sols t
           in mayLog t' >>= \_ -> loop (n+1) t'

log : Int -> Trie -> IO ()
log n t = let fname = ("data/" ++ show n ++ ".dict")  
              s     = Trie.encode t
          in putStrLn ("writing to: " ++ fname) >>
             writeFile { file = fname, content = s }
