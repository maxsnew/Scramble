module Scramble.Tests.Enum where

import Either (..)
import Set
import String

import Scramble.Trie as Trie
import EnumCheck.Enum (finE, mapE, manyE, Enum)
import EnumCheck.Test as EnumCheck

smallCharE : Enum Char
smallCharE = finE . String.toList <| "abcdefghijklmnopqrstuvwxyz"

stringsE : Enum [String]
stringsE = manyE <| String.fromList `mapE` manyE smallCharE

trieE : Enum Trie.Trie
trieE = Trie.fromList `mapE` stringsE

removeDups = Set.toList . Set.fromList

fromListToListIdTest : EnumCheck.Test [String]
fromListToListIdTest = { run ss = EnumCheck.expectEq (sort . removeDups <| ss)
                                                     (sort . Trie.toList . Trie.fromList <| ss)
                       , src = stringsE
                       }

mmap : (a -> b) -> Maybe a -> Maybe b
mmap f = maybe Nothing (Just . f)

binaryTest : EnumCheck.Test Trie.Trie
binaryTest = { run t = EnumCheck.expectEq (Just . Trie.toList <| t) (mmap Trie.toList . Trie.decode . Trie.encode <| t)
             , src = trieE
             }

resultString : Int -> EnumCheck.Test a -> String
resultString n t = either id id <| EnumCheck.runTest n t

main = plainText . resultString 1000 <| binaryTest
