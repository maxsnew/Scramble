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

trieE : Enum Trie
trieE = Trie.fromList `mapE` stringsE

removeDups = Set.toList . Set.fromList

fromListToListIdTest : EnumCheck.Test [String]
fromListToListIdTest = { run ss = EnumCheck.expectEq (sort . removeDups <| ss)
                                                     (sort . Trie.toList . Trie.fromList <| ss)
                       , src = stringsE
                       }

binaryTest : EnumCheck.Test Trie
binaryTest = { run t = EnumCheck.expectEq (Just t) (Trie.decode . Trie.encode <| t)
             , src = trieE
             }

resultString : Int -> EnumCheck.Test a -> String
resultString = either id id . EnumCheck.runTest

main = plainText . resultString 10000 <| fromListToListIdTest